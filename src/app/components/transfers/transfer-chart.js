import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import additionalXLabel from 'op-worker-gui/utils/chartist/additional-x-label';
import shortHorizontalGrid from 'op-worker-gui/utils/chartist/short-horizontal-grid';
import tooltip from 'op-worker-gui/utils/chartist/tooltip';
import centerLineChart from 'op-worker-gui/utils/chartist/center-line-chart';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';

const {
  Component,
  computed,
  get,
} = Ember;

const EXPECTED_STATS_NUMBER = 12;

export default Component.extend({
  classNames: ['transfer-chart'],

  /**
   * @type {Object}
   * @virtual
   */
  stats: undefined,

  lastUpdateTime: undefined,
  
  /**
   * One of `minute`, `hour`, `day`.
   * @type {string}
   * @virtual
   */
  timeUnit: 'minute',

  _chartValues: [],

  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _statsForTimeUnit: computed('stats', 'timeUnit', function () {
    const {
      stats,
      timeUnit,
    } = this.getProperties('stats', 'timeUnit');
    return get(stats, timeUnit);
  }),

  _statsValues: computed('_statsForTimeUnit', function () {
    const statsValues = _.range(EXPECTED_STATS_NUMBER).map(() => 0);
    const _statsForTimeUnit = this.get('_statsForTimeUnit');
    Object.keys(_statsForTimeUnit).forEach(key => {
      let values = _statsForTimeUnit[key];
      if (values.length < EXPECTED_STATS_NUMBER) {
        values = _.range(EXPECTED_STATS_NUMBER - values.length).map(() => 0)
          .concat(values);
      }
      values.forEach((value, index) => statsValues[index] += value);
    });
    return statsValues;
  }),

  _timePeriod: computed('timeUnit', function () {
    const timeUnit = this.get('timeUnit');
    switch (timeUnit) {
      case 'minute':
        return [5, 'seconds'];
      case 'hour':
        return [5, 'minutes'];      
      default:
      case 'day':
        return [2, 'hours'];
    }
  }),
  
  _timeFormat: computed('timeUnit', function () {
    switch (this.get('timeUnit')) {
      case 'hour':
        return 'HH:mm';
      case 'day':
        return 'DD/MM HH:mm';
      default:
        return 'HH:mm:ss';
    }
  }),

  /**
   * Chartist settings
   * @type {Object}
   */
  _chartOptions: computed(function() {
    return {
      axisY: {
        labelInterpolationFnc: (value) => {
          return bytesToString(value) + '/s';
        }
      },
      low: 0,
      chartPadding: {
        top: 30,
        bottom: 10,
        left: 30,
        right: 30,
      },
      plugins: [
        additionalXLabel(),
        shortHorizontalGrid(),
        centerLineChart(),
        tooltip({
          chartType: 'line',
          rangeInTitle: true,
          topOffset: -17,
        }),
      ],
    };
  }),

  /**
   * Data for chartist
   * @type {computed.Object}
   */
  _chartData: computed('_statsValues', function () {
    let {
      _statsValues,
      _chartValues,
    } = this.getProperties(
      '_statsValues',
      '_chartValues'
    );
    while (_chartValues.length) {
      _chartValues.shift();
    }
    _statsValues.forEach(value => _chartValues.push(value));
    return {
      labels: _.range(1, _chartValues.length + 1).reverse()
        .map(n => this.getChartLabel(n)),
      series: [{
        data: _chartValues,
        tooltipElements: _statsValues.map((value) => [{
          name: 'Throughput',
          value: bytesToString(value) + '/s',
          className: 'ct-series-a-tooltip',
        }]),
        className: 'ct-series-a',
      }],
      lastLabel: this.getChartLabel(0),
    };
  }),

  init() {
    this._super(...arguments);
    this.set('_chartValues', []);
  },

  getChartLabel(offset) {
    let {
      lastUpdateTime,
      _timeFormat,
      _timePeriod,
    } = this.getProperties(
      'lastUpdateTime',
      '_timeFormat',
      '_timePeriod');
    return moment(lastUpdateTime)
      .subtract(offset * _timePeriod[0], _timePeriod[1])
      .format(_timeFormat);
  },
});
