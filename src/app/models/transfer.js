/**
 * Data transfer from multiple sources to single provider
 *
 * @module models/transfer
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
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
} = Ember;

const finishedStatus = [
  'completed',
  'skipped',
  'cancelled',
  'failed',
];

export default Model.extend({
  /**
   * Id of Provider that is destination of this transfer
   */
  destination: attr('string'),
  
  file: belongsTo('file', { async: true, inverse: null }),
  
  /**
   * If true, the transfer is a migration, so the file will be invalidated
   * on `migrationSource` provider after migration completion
   */
  migration: attr('boolean'),
  
  /**
   * Id of provider that will invalidate the file after transfer
   */
  migrationSource: attr('string'),
  
  /**
   * If true, the transfer is in progress (should be in current transfers collection)
   */
  isOngoing: attr('boolean'),
  
  /**
   * Absolute file or directory path that is transferred
   */
  path: attr('string'),
  
  /**
   * One of: dir, file, deleted, unknown
   */
  fileType: attr('string'),
  
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
  dest: computed.reads('destination'),
  userName: computed.reads('systemUser.name'),
  transferredBytes: computed.reads('currentStat.transferredBytes'),
  transferredFiles: computed.reads('currentStat.transferredFiles'),
  
  currentStatError: computed('currentStat.{isSettled,content}', function () {
    return this.get('currentStat.isSettled') && this.get('currentStat.content') == null;
  }),
  
  isCurrent: computed.reads('isOngoing'),
  
  /**
   * @type {string}
   * One of: migration, invalidation, replication
   */
  type: computed('migration', 'destination', function getType() {
    if (this.get('migration')) {
      return this.get('destination') ? 'migration' : 'invalidation';
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
    const property = (type === 'replication' || type === 'migration' && this.get('status') === 'invalidating') ?
      'destination' : 'migrationSource';
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
  isCancelling: computed('_isCancelling', 'status', {
    get() {
      const {
        status,
        _isCancelling,
      } = this.getProperties('_isCancelling', 'status');
      // if transfer is finished, then cancelling is not possible
      return status === 'aborting' ||
        (_isCancelling && finishedStatus.indexOf(status) === -1);
    },
    set(key, value) {
      const status = this.get('status');
      this.set('_isCancelling', value);
      return status === 'aborting' ||
        (value && finishedStatus.indexOf(this.get('status')) === -1);
    },
  }),
  
  //#endregion
});
