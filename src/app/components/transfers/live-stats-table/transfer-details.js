import Ember from 'ember';
import _ from 'lodash';

const {
  Component,
  computed,
  get,
} = Ember;

import reduceArrays from 'ember-cli-onedata-common/utils/reduce-arrays';

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
      return reduceArrays(..._.values(periodStats));
    }
  }),
});
