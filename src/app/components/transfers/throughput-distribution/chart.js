import Ember from 'ember';
import OnePieChart from 'op-worker-gui/components/one-pie-chart';
import _ from 'lodash';
import generateColors from 'op-worker-gui/utils/generate-colors';
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
   * @virtual
   * Space transfers throughput data
   * @type {Array<SpaceTransfer>}
   */
  throughputData: [],

  /**
   * @override
   */
  data: computed('providers.[]', 'throughputData.[]', function () {
    const {
      providers,
      throughputData,
    } = this.getProperties('providers', 'throughputData');
    const colors = generateColors(throughputData.length);
    return throughputData.map((transfer, index) => (Ember.Object.create({
      id: String(index),
      label: get(_.find(
        providers,
        (p) => get(p, 'id') === transfer.providerId
      ), 'name'),
      value: transfer.bytesPerSec,
      color: colors[index],
    })));
  }),

  /**
   * @override
   */
  formatValue(value) {
    return bytesToString(value) + '/s';
  },
});
