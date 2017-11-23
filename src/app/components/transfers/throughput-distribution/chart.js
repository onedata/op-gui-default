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
  providers: [],

  /**
   * Predefined providers colors
   * @virtual
   * @type {Object}
   */
  providersColors: {},

  /**
   * @virtual
   * Space transfers throughput data
   * @type {Ember.Array<SpaceTransfer>}
   */
  throughputData: [],

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
        value: transfer.bytesPerSec,
        color: providersColors[get(transfer, 'providerId')],
      }));
    }
  ),

  /**
   * @override
   */
  formatValue(value) {
    return bytesToString(value) + '/s';
  },
});
