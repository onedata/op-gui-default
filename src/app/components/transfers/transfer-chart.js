import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import additionalXLabel from 'op-worker-gui/utils/chartist/additional-x-label';
import shortHorizontalGrid from 'op-worker-gui/utils/chartist/short-horizontal-grid';
import tooltip from 'op-worker-gui/utils/chartist/tooltip';
import centerLineChart from 'op-worker-gui/utils/chartist/center-line-chart';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import axisLabels from 'op-worker-gui/utils/chartist/axis-labels';

const {
  Component,
  computed,
  get,
  observer,
  on,
} = Ember;

const EXPECTED_STATS_NUMBER = 12;

export default Component.extend({
  classNames: ['transfer-chart'],

  /**
   * @type {Object}
   * @virtual
   */
  stats: undefined,

  /**
   * Last update time
   * @type {Ember.ComputedProperty<Date>}
   */
  _lastUpdateTime: computed('_statsContainerForTimeUnit.content.date', function () {
    const _statsContainerForTimeUnit = this.get('_statsContainerForTimeUnit');
    const date = get(_statsContainerForTimeUnit, 'content.date');
    return date ? new Date(date) : new Date();
  }),
  
  /**
   * One of `minute`, `hour`, `day`.
   * @type {string}
   * @virtual
   */
  timeUnit: 'minute',

  /**
   * Array of actual chart values.
   * @type {Array<number>}
   */
  _chartValues: [],

  /**
   * Object with stats for specified time unit.
   * @type {Ember.ComputedProperty.Object}
   */
  _statsContainerForTimeUnit: computed('stats', 'timeUnit', function () {
    const {
      stats,
      timeUnit,
    } = this.getProperties('stats', 'timeUnit');
    return get(stats, timeUnit);
  }),

  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _statsForTimeUnit: {},

  /**
   * Stats values for time unit. Values from this array will be copied
   * to the _chartValues.
   * @type {Ember.ComputedProperty<Array<number>>}
   */
  _statsValues: computed('_statsForTimeUnit', function () {
    const statsValues = _.range(EXPECTED_STATS_NUMBER).map(() => 0);
    const _statsForTimeUnit = this.get('_statsForTimeUnit');
    console.log(_statsForTimeUnit);
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

  /**
   * Chart time period
   * @type {Ember.ComputedProperty<Array<any>>}
   */
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
  
  /**
   * Chart time format
   * @type {Ember.ComputedProperty<string>}
   */
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
  _chartOptions: {
    axisY: {
      labelInterpolationFnc: (value) => {
        return bytesToString(value) + '/s';
      }
    },
    low: 0,
    chartPadding: {
      top: 30,
      bottom: 30,
      left: 60,
      right: 60,
    },
    plugins: [
      additionalXLabel(),
      shortHorizontalGrid(),
      centerLineChart(),
      axisLabels({
        xLabel: 'Time',
        yLabel: 'Throughput',
      }),
      tooltip({
        chartType: 'line',
        rangeInTitle: true,
        topOffset: -17,
      }),
    ],
  },

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

  timeUnitObserver: on('init', observer('timeUnit', function () {
    this.set(
      '_statsForTimeUnit',
      computed.oneWay(
        `_statsContainerForTimeUnit.content.${this.get('timeUnit')}`
      )
    );
  })),

  init() {
    this._super(...arguments);
    this.set('_chartValues', []);
  },

  getChartLabel(offset) {
    let {
      _lastUpdateTime,
      _timeFormat,
      _timePeriod,
    } = this.getProperties(
      '_lastUpdateTime',
      '_timeFormat',
      '_timePeriod');
    return moment(_lastUpdateTime)
      .subtract(offset * _timePeriod[0], _timePeriod[1])
      .format(_timeFormat);
  },
});
