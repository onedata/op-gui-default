import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import additionalXLabel from 'op-worker-gui/utils/chartist/additional-x-label';
import shortHorizontalGrid from 'op-worker-gui/utils/chartist/short-horizontal-grid';
import tooltip from 'op-worker-gui/utils/chartist/tooltip';
import centerLineChart from 'op-worker-gui/utils/chartist/center-line-chart';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import axisLabels from 'op-worker-gui/utils/chartist/axis-labels';
import TransferTimeStatUpdater from 'op-worker-gui/utils/transfer-time-stat-updater';

const {
  Component,
  computed,
  get,
} = Ember;

const EXPECTED_STATS_NUMBER = 12;
const MINUTE_STATS_NUMBER = 12;
const HOUR_STATS_NUMBER = 60;
const DAY_STATS_NUMBER = 24;

export default Component.extend({
  classNames: ['transfer-chart'],
  
  /**
   * @virtual
   * @type {Transfer}
   */
  transfer: undefined,

  /**
   * Last update time (async -> _timeStatForUnit)
   * @type {Ember.ComputedProperty<Date>}
   */
  _lastUpdateTime: computed('_timeStatForUnit.timestamp', function () {
    const _timeStatForUnit = this.get('_timeStatForUnit');
    const date = get(_timeStatForUnit, 'timestamp');
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
   * Initialized when stat record is available (after init)
   * @type {TransferTimeStatUpdater}
   */
  updater: undefined,
  
  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _stats: computed.reads('_timeStatForUnit.content.stats'),

  /**
   * A number of stats, that should be considered as a single chart value
   * @type {Ember.ComputedProperty<number>}
   */
  _statsUnitsPerChartValue: computed('timeUnit', function () {
    let statsPerUnit;
    switch (this.get('timeUnit')) {
      case 'day':
        statsPerUnit = DAY_STATS_NUMBER;
        break;
      case 'hour':
        statsPerUnit = HOUR_STATS_NUMBER;
        break;
      default:
      case 'minute':
        statsPerUnit = MINUTE_STATS_NUMBER;
    }
    return statsPerUnit / EXPECTED_STATS_NUMBER;
  }),

  // FIXME: this should be auto updated
  /**
   * Object with stats for specified time unit.
   * @type {Ember.ComputedProperty.Object}
   */
  _timeStatForUnit: computed('transfer', 'timeUnit', function () {
    const {
      transfer,
      timeUnit,
    } = this.getProperties('transfer', 'timeUnit');
    return get(transfer, `${timeUnit}Stat`);
  }),
  
  /**
   * Stats values for time unit in order: from oldest to newest (inverts backend
   * order). Values from this array will be copied to the _chartValues.
   * (async -> _stats)
   * @type {Ember.ComputedProperty<Array<number>>}
   */
  _statsValues: computed('_stats', '_statsUnitsPerChartValue', function () {
    const {
      _stats,
      _statsUnitsPerChartValue,
    } = this.getProperties('_stats', '_statsUnitsPerChartValue');
    const inputStatsValuesNumber = EXPECTED_STATS_NUMBER * _statsUnitsPerChartValue;
    const statsValues = _.range(inputStatsValuesNumber).map(() => 0);

    Object.keys(_stats).forEach(key => {
      let values = _stats[key];
      if (values.length < inputStatsValuesNumber) {
        values = _.range(inputStatsValuesNumber - values.length).map(() => 0)
          .concat(values);
      }
      values.forEach((value, index) => statsValues[index] += value);
    });
    const scaledStats = [];
    for (let i = 0; i < statsValues.length; i += _statsUnitsPerChartValue) {
      let singleChartStat = 0;
      for (let j = 0; j < _statsUnitsPerChartValue; j++) {
        singleChartStat += statsValues[i + j];
      }
      scaledStats.push(singleChartStat / _statsUnitsPerChartValue);
    }
    return scaledStats.slice().reverse();
  }),

  /**
   * Chart time period
   * @type {Ember.ComputedProperty<Array<any>>}
   */
  _timePeriod: computed('timeUnit', function () {
    const timeUnit = this.get('timeUnit');
    switch (timeUnit) {
      case 'minute':
        return [60 / EXPECTED_STATS_NUMBER, 'seconds'];
      case 'hour':
        return [60 / EXPECTED_STATS_NUMBER, 'minutes'];      
      default:
      case 'day':
        return [24 / EXPECTED_STATS_NUMBER, 'hours'];
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
      left: 50,
      right: 50,
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
   * Data for chartist (async -> _statsValues)
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

  _updaterEnabled: true,
  
  init() {
    this._super(...arguments);
    this.set('_chartValues', []);
    
    this.get('_timeStatForUnit').then(timeStat => {
      const updater = TransferTimeStatUpdater.create({
        isEnabled: this.get('_updaterEnabled'),
        timeStat,
      });
      this.set('updater', updater);
    });
  },
  
  willDestroyElement() {
    try {
      const updater = this.get('updater');
      if (updater) {
        updater.destroy();
      }
    } finally {
      this._super(...arguments);
    }
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
