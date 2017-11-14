import DS from 'ember-data';
import Ember from 'ember';

import TransferRuntimeMixin from 'op-worker-gui/mixins/models/transfer-runtime';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  Model,
  attr,
  belongsTo,
} = DS;

const {
  computed,
} = Ember;

export default Model.extend(TransferRuntimeMixin, {
  status: attr('string'),
  destination: attr('string'),
  // FIXME: computed property with file name?
  path: attr('string'),
  
  /**
   * One of: dir, file, deleted, unknown
   */
  fileType: attr('string'),
  
  startTime: attr('number'),
  finishTime: attr('number'),
  
  // TODO: this should be changed to systemUserId
  systemUserId: attr('string'),
  
  currentStat: belongsTo('transfer-current-stat'),

  minuteStat: belongsTo('transfer-time-stat'),
  hourStat: belongsTo('transfer-time-stat'),
  dayStat: belongsTo('transfer-time-stat'),
  monthStat: belongsTo('transfer-time-stat'),
  
  /**
   * @type {Ember.ComputedProperty<PromiseObject<SystemProvider>>}
   */
  systemUser: computed('id', 'systemUserId', function () {
    const {
      id,
      systemUserId,
    } = this.getProperties('id', 'systemUserId');
    return PromiseObject.create({
      promise: this.store.queryRecord('system-user', {
        id: systemUserId,
        context: {
          od_space: id,
        },
      })
    });
  }),
  
  // Flattening some properties for view
  tableDataIsLoaded: computed(
    'isLoaded',
    'currentStat.isLoaded',
    'systemUser.isLoaded',
    function () {
      return this.get('isLoaded') &&
        this.get('currentStat.isLoaded') &&
        this.get('systemUser.isLoaded');
    }
  ),
  
  dest: computed.reads('destination'),
  bytesPerSec: computed.reads('currentStat.bytesPerSec'),
  userName: computed.reads('systemUser.name'),
});

// -- FIXME: mocks --

const ONE_GB = Math.pow(1024, 3);

import mockBelongsTo from 'op-worker-gui/utils/mock-belongs-to';

import {
  mockRecord as mockCurrentStat,
} from 'op-worker-gui/models/transfer-current-stat';

import {
  mockRecord as mockTimeStat,
} from 'op-worker-gui/models/transfer-time-stat';

// FIXME: outdated API
export const mock1 = {
  destination: 'p2',
  fileName: 'file1',
  userName: 'John Smith',
  totalBytes: 1.5 * ONE_GB,
  startedAt: new Date().toISOString(),
  currentStat: mockBelongsTo(mockCurrentStat(true, 3*ONE_GB, 0.2*ONE_GB)),
  minuteStat: mockBelongsTo(mockTimeStat('minute')),
  hourStat: mockBelongsTo(mockTimeStat('hour')),
  dayStat: mockBelongsTo(mockTimeStat('day')),
};

// FIXME: outdated API
export const mock2 = {
  destination: 'p3',
  fileName: 'file2',
  userName: 'David Grohlton',
  totalBytes: 3 * ONE_GB,
  startedAt: new Date().toISOString(),
  currentStat: mockBelongsTo(mockCurrentStat(true, 2 * ONE_GB, 0.3 * ONE_GB)),
  minuteStat: mockBelongsTo(mockTimeStat('minute')),
  hourStat: mockBelongsTo(mockTimeStat('hour')),
  dayStat: mockBelongsTo(mockTimeStat('day')),
};
