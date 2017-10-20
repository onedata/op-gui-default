import Ember from 'ember';

const {
  Component,
  computed,
  get,
} = Ember;

import _ from 'lodash';

export default Component.extend({
  /**
   * @virtual
   * Currently injecting all info about transferred data - it is fetched outside
   */
  transferStats: undefined,
  
  // FIXME: time span of histogram - eg. currently selected
  period: 'hour',
  
  // FIXME: eg. computedProperty to get data for chosen period
  /**
   * Array of segments of total transferred bytes for period
   * @type {Array<number>}
   */
  statsForPeriod: computed('transferStats', 'period', function () {
    const {
      transferStats,
      period,
    } = this.getProperties('transferStats', 'period');
    if (period && transferStats) {
      const periodStats = get(transferStats, period);
      return sumArrays(_.values(periodStats));
    }
  }),
});

/**
 * Reduce multiple arrays into one by summing values on the same positions
 * Eg.: `[[1,2,3], [10,20,30], [100,200,300]]` -> `[111, 222, 333]`
 * 
 * @param {Array<Array<number>>} arrays 
 * @returns {Array<number>}
 */
function sumArrays(arrays) {
  const arrayCount = arrays.length;
  const arrayLength = arrays[0].length;
  const buf = [];
  for (let pos = 0; pos < arrayCount; pos += 1) {
    let posSum = 0;
    for (let arrayNum = 0; arrayNum < arrayLength; arrayNum += 1) {
      posSum += (arrays[arrayNum][pos] || 0);
    }
    buf.push(posSum);
  }
  return buf;
}
