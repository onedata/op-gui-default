import DS from 'ember-data';

import TransferRuntimeMixin from 'op-worker-gui/mixins/models/transfer-runtime';

const {
  Model,
  // FIXME: create attrs for everything
  attr,
} = DS;

export default Model.extend(TransferRuntimeMixin, {
  destination: attr('string'),
  fileName: attr('string'),
  userName: attr('string'),
  totalBytes: attr('number'),
  startedAt: attr('string'), // TODO: make ISO date type?
  
  /**
   * FIXME: object {
   *   hour: {
   *     p1: [], // 1-60 number values, last value is most recent
   *     p2: [],
   *   },
   *   day: {
   *     p1: [], // 1-24 number values
   *     p2: [],
   *   },
   *   month: {
   *     p1: [], // 1-30 number values - each for day
   *     p2: [],
   *   },
   * }
   */
  stats: attr('object'),
});
