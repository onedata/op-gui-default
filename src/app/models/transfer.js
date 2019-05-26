/**
 * Data transfer from multiple sources to single provider
 *
 * @module models/transfer
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';
import Ember from 'ember';

import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  Model,
  attr,
  belongsTo,
} = DS;

const {
  computed,
  RSVP: { resolve },
} = Ember;

const finishedStatuses = [
  'completed',
  'skipped',
  'cancelled',
  'failed',
];

export default Model.extend({
  index: attr('string'),
  
  /**
   * Id of Provider that is destination of this transfer
   */
  replicatingProvider: attr('string'),
  
  /**
   * Id of provider that will evict the file after transfer
   */
  evictingProvider: attr('string'),
  
  /**
   * If true, the transfer is in progress (should be in current transfers collection)
   */
  isOngoing: attr('boolean'),
    
  queryParams: attr('object'),
  
  /**
   * One of: dir, file, deleted, index, unknown
   */
  dataSourceType: attr('string'),
  
  /**
   * If data type is file/dir/deleted, then it's absolute path of file.
   * If data type is index then it's index name.
   */
  dataSourceName: attr('string'),
  
  dataSourceIdentifier: attr('string'),
  
  /**
   * Id of record that holds data that is transferred. Depends of data type:
   * - for file, dir, deleted: id of file record
   * - for index: id od db-index record
   */
  dataSourceRecord: computed('dataSourceType', 'dataSourceIdentifier', function dataSourceRecord() {
    const {
      store,
      dataSourceType,
      dataSourceIdentifier,
    } = this.getProperties('store', 'dataSourceType', 'dataSourceIdentifier');
    let promise;
    switch (dataSourceType) {
      case 'dir':
      case 'file':
        promise = store.findRecord('file', dataSourceIdentifier);
        break;
      case 'index':
        promise = store.findRecord('db-index', dataSourceIdentifier);
        break;
      default:
        promise = resolve();
        break;
    }
    return PromiseObject.create({ promise });
  }),
  
  /**
   * UNIX timestamp seconds format
   */
  scheduleTime: attr('number'),
  
  /**
   * UNIX timestamp seconds format
   */
  startTime: attr('number'),
  
  /**
   * Non-empty only if transfer is not current (`isCurrent`)
   * UNIX timestamp seconds format
   */
  finishTime: attr('number'),
  
  systemUserId: attr('string'),
  
  currentStat: belongsTo('transfer-current-stat'),

  minuteStat: belongsTo('transfer-time-stat'),
  hourStat: belongsTo('transfer-time-stat'),
  dayStat: belongsTo('transfer-time-stat'),
  monthStat: belongsTo('transfer-time-stat'),
    
  /**
   * Space in which this transfer is done
   */
  space: belongsTo('space', { async: true, inverse: null }),
 
  /**
   * Index name
   */
  indexName: computed('dataSourceName', 'dataSourceType', function path() {
    const dataSourceType = this.get('dataSourceType');
    if (dataSourceType === 'index') {
      return this.get('dataSourceName');
    }
  }),
  
  /**
   * Absolute file or directory path that is transferred
   */
  path: computed('dataSourceName', 'dataSourceType', function path() {
    const dataSourceType = this.get('dataSourceType');
    if (dataSourceType === 'file' || dataSourceType === 'dir' || dataSourceType === 'deleted') {
      return this.get('dataSourceName');
    }
  }),
  
  /**
   * @type {Ember.ComputedProperty<PromiseObject<SystemProvider>>}
   */
  systemUser: computed('space', 'systemUserId', function () {
    const systemUserId = this.get('systemUserId');
    // the relation id should exist always if record exists
    const spaceId = this.belongsTo('space').id();
    if (spaceId != null) {
      return PromiseObject.create({
        promise: this.store.queryRecord('system-user', {
          id: systemUserId,
          context: {
            od_space: spaceId,
          },
        })
      }); 
    }
  }),
  
  // Flattening some properties for view
  tableDataIsLoaded: computed(
    'isLoaded',
    'currentStat.isLoaded',
    'systemUser.isLoaded',
    '_completedReloading',
    function () {
      return this.get('isLoaded') &&
        this.get('currentStat.isLoaded') &&
        this.get('systemUser.isLoaded') &&
        !this.get('_completedReloading');
    }
  ),
  
  status: computed.reads('currentStat.status'),
  dest: computed.reads('replicatingProvider'),
  userName: computed.reads('systemUser.name'),
  replicatedBytes: computed.reads('currentStat.replicatedBytes'),
  replicatedFiles: computed.reads('currentStat.replicatedFiles'),
  evictedFiles: computed.reads('currentStat.evictedFiles'),
  
  currentStatError: computed('currentStat.{isSettled,content}', function () {
    return this.get('currentStat.isSettled') && this.get('currentStat.content') == null;
  }),
  
  isCurrent: computed.reads('isOngoing'),

  /**
   * @type {boolean}
   */
  isEnded: computed('status', function () {
    return finishedStatuses.indexOf(this.get('status')) !== -1;
  }),
  
  /**
   * @type {string}
   * One of: migration, eviction, replication
   */
  type: computed('evictingProvider', 'replicatingProvider', function getType() {
    const {
      replicatingProvider,
      evictingProvider,
    } = this.getProperties('replicatingProvider', 'evictingProvider');
    if (evictingProvider) {
      return replicatingProvider ? 'migration' : 'eviction';
    } else {
      return 'replication';
    }
  }),
  
  /**
   * True if statistics of this provider are stored on the currently opened provider
   * @param {string} providerId ID of current provider
   * @returns {boolean}
   */
  getIsLocal(providerId) {
    const type = this.get('type');
    const property = (type === 'replication' || type === 'migration' && this.get('status') === 'evicting') ?
      'replicatingProvider' : 'evictingProvider';
    return this.get(property) === providerId;
  },

  //#region Runtime properties

  /**
   * Helper property for `isCancelling` computed property.
   * @type {boolean}
   */
  _isCancelling: false,
  
  /**
   * If true, user has invoked transfer cancellation
   * @type {boolean}
   */
  isCancelling: computed('_isCancelling', 'status', 'isEnded', {
    get() {
      const {
        status,
        isEnded,
        _isCancelling,
      } = this.getProperties('_isCancelling', 'isEnded', 'status');
      // if transfer is finished, then cancelling is not possible
      return status === 'aborting' || (_isCancelling && !isEnded);
    },
    set(key, value) {
      const {
        status,
        isEnded,
      } = this.getProperties('status', 'isEnded');
      this.set('_isCancelling', value);
      return status === 'aborting' || (value && !isEnded);
    },
  }),
  
  //#endregion
});
