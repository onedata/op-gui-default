import DS from 'ember-data';
import Ember from 'ember';
import isDefaultMixinFactory from 'ember-cli-onedata-common/mixin-factories/models/is-default';
import ReplacingChunksArray from 'ember-cli-onedata-common/utils/replacing-chunks-array';

import FakeListRecordRelation from 'op-worker-gui/utils/fake-list-record-relation';


const {
  attr,
  belongsTo,
} = DS;

const {
  computed,
  RSVP: { Promise },
  inject: { service },
} = Ember;

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
  
  /**
   * @type {Ember.ComputedProperty<FakeListRecordRelation>}
   */
  scheduledTransferList: computedTransfersList('waiting'),
  
  /**
   * @type {Ember.ComputedProperty<FakeListRecordRelation>}
   */
  currentTransferList: computedTransfersList('ongoing'),
  
  /**
   * @type {Ember.ComputedProperty<FakeListRecordRelation>}
   */
  completedTransferList: computedTransfersList('ended'),
    
  /**
   * Fetch partial list of space transfer records
   * @param {string} type one of: waiting, ongoing, ended
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

/**
 * @param {string} type one of: waiting, ongoing, ended
 */
function computedTransfersList(type) {
  return computed(function() {
    const initChunksArray = ReplacingChunksArray.create({
      fetch: (...args) => this.fetchTransfers(type, ...args),
      startIndex: 0,
      endIndex: 50,
      indexMargin: 10,
    });
    return FakeListRecordRelation.create({ initChunksArray });
  });
}
