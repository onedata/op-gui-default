import DS from 'ember-data';
import Ember from 'ember';
import isDefaultMixinFactory from 'ember-cli-onedata-common/mixin-factories/models/is-default';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';
import ReplacingChunksArray from 'ember-cli-onedata-common/utils/replacing-chunks-array';

const {
  attr,
  belongsTo,
} = DS;

const {
  computed,
  Object: EmberObject,
  RSVP: { Promise },
  inject: { service },
  get,
} = Ember;

class FakeListHasMany {
  /**
   * @param {SliceArray} list 
   * @param {Ember.Array} list.sourceArray
   */
  constructor(list) {
    this.sourceArray = list.get('sourceArray');
  }
  ids() {
    return this.sourceArray.mapBy('id').toArray();
  }
}

const FakeListRecord = EmberObject.extend({
  /**
   * @virtual 
   * @type {ReplacingChunksArray}
   */
  initChunksArray: undefined,
  
  /**
   * @type {FakeListHasMany}
   */
  _listHasMany: undefined,
  
  list: computed(function () {
    const _chunksArray = this.get('_chunksArray');
    return PromiseObject.create({ promise: Promise.resolve(_chunksArray) });
  }),
  
  chunksArray: computed.reads('_chunksArray'),
  
  hasMany(relation) {
    if (relation === 'list') {
      return this.get('_listHasMany');
    } else {
      throw new Error('FakeListRecord: hasMany for non-"list" relation is not implemented');
    }
  },
  
  reload() {
    return this.get('_chunksArray')
      .reload(...arguments)
      .then(() => this);  
  },
  
  init() {
    this._super(...arguments);
    const _chunksArray = this.set('_chunksArray', this.get('initChunksArray'));
    this.set('_listHasMany', new FakeListHasMany(_chunksArray));
  },
});

const FakeListRecordRelation = PromiseObject.extend({
  isLoading: computed.reads('isPending'),
  isLoaded: computed.not('isLoading'),
  reload() {
    return this.get('_fakeListRecord').reload(...arguments);
  },
  init() {
    this._super(...arguments);
    const _fakeListRecord = this.set('_fakeListRecord', FakeListRecord.create({
      initChunksArray: this.get('initChunksArray'),
    }));
    this.set(
      'promise',
      get(_fakeListRecord, 'chunksArray.initialLoad')
        .then(() => _fakeListRecord)
    );
  },
});

/**
 * A configuration of a space - entry point for all options
 * that can be reached from "spaces" button in primary sidebar.
 *
 * @module models/space
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(isDefaultMixinFactory('defaultSpaceId'), {
  oneproviderServer: service(),
  
  /** User specified name of space that will be exposed in GUI */
  name: attr('string'),

  hasViewPrivilege: attr('boolean'),

  /*** RELATIONS */

  user: belongsTo('user', { async: true }),

  /** A root directory with space files. It must be a dir-type File! */
  rootDir: belongsTo('file', { async: true }),

  /** Collection of users permissions - effectively all rows in permissions table */
  userList: belongsTo('space-user-list', { async: true }),

  /** Collection of group permissions - effectively all rows in permissions table */
  groupList: belongsTo('space-group-list', { async: true }),
  
  onTheFlyTransferList: belongsTo('space-on-the-fly-transfer-list', { async: true, inverse: null }),
  
  providerList: belongsTo('space-provider-list', { async: true, inverse: null }),

  transferOnTheFlyStat: belongsTo('space-transfer-stat', { async: true, inverse: null }),
  transferJobStat: belongsTo('space-transfer-stat', { async: true, inverse: null }),
  transferAllStat: belongsTo('space-transfer-stat', { async: true, inverse: null }),
  transferLinkState: belongsTo('space-transfer-link-state', { async: true, inverse: null }),

  /**
   * On-the-fly, job and all transfer stats for each provider.
   * It is an object in format:
   * ``
   * {
   *   provider1Id: {
   *       onTheFlyStat: id_of_space-transfer-stat,
   *       jobStat: id_of_space-transfer-stat,
   *       allStat: id_of_space-transfer-stat,
   *   },
   *   provider2Id: {
   *       ...like above...
   *   },
   *   ...
   * }
   * ``
   */
  transferProviderStat: attr('object'),
    
  init() {
    this._createTransferLists();
  },
  
  _createTransferLists() {
    ['scheduled', 'current', 'completed'].forEach(type => {
      const chunksArray = ReplacingChunksArray.create({
        fetch: (...args) => this.fetchTransfers(type, ...args),
        startIndex: 0,
        endIndex: 50,
        indexMargin: 10,
      });
      this.set(`${type}TransferList`, FakeListRecordRelation.create({
        initChunksArray: chunksArray,
      }));
    });
  },
  
  /**
   * Fetch partial list of space transfer records
   * @param {string} type one of: scheduled, current, completed
   * @returns {Promise<object>} promise of RPC request with transfers list
   */
  fetchTransfers(type, startFromIndex, size, offset) {
    const {
      oneproviderServer,
      store,
    } = this.getProperties('oneproviderServer', 'store');
    return oneproviderServer.getSpaceTransfers(
      this.get('id'),
      type,
      startFromIndex,
      size,
      offset
    ).then(({ list }) =>
      Promise.all(list.map(id => store.findRecord('transfer', id)))
    );
  },
});
