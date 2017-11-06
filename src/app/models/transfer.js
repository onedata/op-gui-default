import DS from 'ember-data';

import TransferRuntimeMixin from 'op-worker-gui/mixins/models/transfer-runtime';

const {
  Model,
  attr,
  belongsTo,
} = DS;

export default Model.extend(TransferRuntimeMixin, {
  destination: attr('string'),
  fileName: attr('string'),
  userName: attr('string'),
  totalBytes: attr('number'),
  startedAt: attr('string'), // TODO: make ISO date type?

  currentStat: belongsTo('transfer-current-stat'),

  minuteStat: belongsTo('transfer-time-stat'),
  hourStat: belongsTo('transfer-time-stat'),
  dayStat: belongsTo('transfer-time-stat'),
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
