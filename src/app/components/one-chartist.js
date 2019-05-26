/**
 * Extension of chartist-chart component from ember-chartist package. It adds 
 * correct detaching of destroyed charts.
 * 
 * @module components/one-chartist
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import ChartistChart from './chartist-chart';

export default ChartistChart.extend({
  willDestroyElement() {
    try {
      this.get('chart').detach();
    } finally {
      this._super(...arguments);
    }
  },
});
