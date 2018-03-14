/**
 * A component that creates pie chart of transfers input/output throughput
 * sum per provider.
 * 
 * @module components/transfers/throughput-distribution/chart
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import OnePieChart from 'op-worker-gui/components/one-pie-chart';
import _ from 'lodash';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import layout from 'op-worker-gui/templates/components/one-pie-chart';

const {
  get,
  computed,
} = Ember;

export default OnePieChart.extend({
  layout,
  className: ['throughput-distribution-chart'],

  /**
   * @virtual
   * Array of all providers, that takes part in transfers.
   * @type {Array<Provider>}
   */
  providers: Object.freeze([]),

  /**
   * Predefined providers colors
   * @virtual
   * @type {Object}
   */
  providersColors: Object.freeze({}),

  /**
   * @virtual
   * Space transfers throughput data
   * @type {Ember.Array<SpaceTransfer>}
   */
  throughputData: Object.freeze([]),

  /**
   * @override
   */
  data: computed(
    'providers.[]',
    'throughputData.@each.bytesPerSec',
    'providersColors',
    function () {
      const {
        providers,
        throughputData,
        providersColors,
      } = this.getProperties('providers', 'throughputData', 'providersColors');
      return throughputData.map((transfer, index) => Ember.Object.create({
        id: String(index),
        label: get(_.find(
          providers,
          (p) => get(p, 'id') === get(transfer, 'providerId')
        ), 'name'),
        value: get(transfer, 'bytesPerSec'),
        color: providersColors[get(transfer, 'providerId')],
      }));
    }
  ),

  /**
   * @override
   */
  formatValue(value) {
    return bytesToString(value, { format: 'bit' }) + 'ps';
  },
});
