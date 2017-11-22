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
import customCss from 'op-worker-gui/utils/chartist/custom-css';
import Color from 'npm:color';

const {
  Component,
  computed,
  get,
  inject: {
    service,
  },
} = Ember;

const EXPECTED_STATS_NUMBER = 12;
const MINUTE_STATS_NUMBER = 12;
const HOUR_STATS_NUMBER = 60;
const DAY_STATS_NUMBER = 24;
const CHART_BACKGROUND_COLOR = '#e3e3e3';
const I18N_PREFIX = 'components.transfers.transferChart.';

export default Component.extend({
  classNames: ['transfer-chart'],

  i18n: service(),
  
  /**
   * @virtual
   * @type {Transfer}
   */
  transfer: undefined,

  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,

  /**
   * Last update time (async -> _timeStatForUnit)
   * @type {Ember.ComputedProperty<Date>}
   */
  _lastUpdateTime: computed('_timeStatForUnit.timestamp', 'transfer.isOngoing', function () {
    const {
      _timeStatForUnit,
      transfer,
    } = this.getProperties('_timeStatForUnit', 'transfer');
    if (transfer.get('isOngoing')) {
      const date = get(_timeStatForUnit, 'timestamp');
      return date ? new Date(date) : new Date();
    } else {
      return new Date(transfer.get('finishTime'));
    }
  }),
  
  /**
   * One of `minute`, `hour`, `day`.
   * @type {string}
   * @virtual
   */
  timeUnit: 'minute',

  /**
   * Array of actual chart values.
   * @type {Array<Array<number>>}
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
   * Last stat timestamp
   * @type {Ember.ComputedProperty<number>}
   */
  _statsTimestamp: computed('_timeStatForUnit.content.timestamp', 'transfer.isOngoing', function () {
    const transfer = this.get('transfer');
    if (transfer.get('isOngoing')) {
      return this.get('_timeStatForUnit.content.timestamp');
    } else {
      return transfer.get('finishTime');
    }
  }),

  /**
   * @type {Ember.ComputedProperty<number>}
   */
  _transferStartTime: computed.reads('transfer.startTime'),

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
   * Sorted provider ids.
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  _sortedProvidersIds: computed('_stats', function () {
    return Object.keys(this.get('_stats')).sort();
  }),

  /**
   * Colors used to color each providers' series
   * @virtual
   * @type {Ember.ComputedProperty<Object>}
   */
  providersColors: {},
  
  /**
   * Stats values for time unit in order: from oldest to newest (inverts backend
   * order). Values from this array will be copied to the _chartValues.
   * (async -> _stats)
   * @type {Ember.ComputedProperty<Array<number>>}
   */
  _statsValues: computed('_sortedProvidersIds', '_statsUnitsPerChartValue', function () {
    const {
      _stats,
      _statsUnitsPerChartValue,
      _sortedProvidersIds,
    } = this.getProperties(
      '_stats',
      '_statsUnitsPerChartValue',
      '_sortedProvidersIds'
    );
    const inputStatsValuesNumber = EXPECTED_STATS_NUMBER * _statsUnitsPerChartValue;
    const statsValues = [];

    _sortedProvidersIds.forEach(key => {
      let values = _stats[key];
      if (values.length < inputStatsValuesNumber) {
        values = _.range(inputStatsValuesNumber - values.length).map(() => 0)
          .concat(values);
      }
      const scaledValues = [];
      for (let i = 0; i < values.length; i += _statsUnitsPerChartValue) {
        let singleChartStat = 0;
        for (let j = 0; j < _statsUnitsPerChartValue; j++) {
          singleChartStat += values[i + j];
        }
        scaledValues.push(this._scaleStatValue(singleChartStat, scaledValues.length));
      }
      statsValues.push(scaledValues.reverse());
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
  _chartOptions: computed(function() {
    const i18n = this.get('i18n');
    return {
      axisY: {
        labelInterpolationFnc: (value) => {
          return bytesToString(value) + '/s';
        }
      },
      low: 0,
      showArea: true,
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
          xLabel: i18n.t(I18N_PREFIX + 'time'),
          yLabel: i18n.t(I18N_PREFIX + 'throughput'),
        }),
        tooltip({
          chartType: 'line',
          rangeInTitle: true,
          topOffset: -17,
        }),
        customCss({
          filterBySeriesIndex: true,
        }),
      ],
    };
  }),

  /**
   * Data for chartist (async -> _statsValues)
   * @type {computed.Object}
   */
  _chartData: computed('_statsValues', 'providers.@each.name', function () {
    let {
      _statsValues,
      _chartValues,
      _sortedProvidersIds,
      providersColors,
      providers,
    } = this.getProperties(
      '_statsValues',
      '_chartValues',
      '_sortedProvidersIds',
      'providersColors',
      'providers'
    );
    // clearing out old chart values
    _chartValues.forEach(providerValues => {
      while (providerValues.length) {
        providerValues.shift();
      }
    });
    // extending chart values to hold all needed providers
    while (_chartValues.length < _statsValues.length) {
      _chartValues.push([]);
    }
    // calculating new chart values
    const valuesSumArray = _.times(EXPECTED_STATS_NUMBER, _.constant(0));
    _statsValues.slice(0).reverse().forEach((providerValues, index) => {
      providerValues.forEach((value, index2) => valuesSumArray[index2] += value);
      _chartValues[_chartValues.length - index - 1].push(...(valuesSumArray));
    });
    // creating tooltips
    const tooltipElements = _chartValues[0].map((value, index) => {
      return _sortedProvidersIds.map((providerId, providerIndex) => {
        const provider =
          _.find(providers, (provider) => provider.get('id') === providerId) || {};
        const providerName = get(provider, 'name') || providerId;
        return {
          name: providerName.length > 10 ?
              providerName.substring(0, 8) + '...' : providerName,
          value: bytesToString(_chartValues[providerIndex][index]) + '/s',
          className: 'ct-tooltip-entry',
          cssString: 'border-color: ' + providersColors[providerId],
        };
      });
    });
    // setting colors
    const customCss = _sortedProvidersIds.map((providerId) => {
      const color = providersColors[providerId];
      const colorMixedWithBackgr =
        new Color(color).mix(new Color(CHART_BACKGROUND_COLOR), 0.4).hex();
      return _.times(EXPECTED_STATS_NUMBER, _.constant({
        line: {
          stroke: color,
        },
        point: {
          stroke: color,
        },
        area: {
          fill: colorMixedWithBackgr,
        }
      }));
    });
    // creating chart data object
    return {
      labels: _.range(1, _chartValues[0].length + 1).reverse()
        .map(n => this._getChartLabel(n)),
      series: _chartValues.map((providerValues) => ({
        data: providerValues,
        tooltipElements,
      })),
      lastLabel: this._getChartLabel(0),
      customCss,
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

  /**
   * Returns chart label for specified time offset (time step number)
   * @param {number} offset
   * @returns {string}
   */
  _getChartLabel(offset) {
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

  /**
   * Calculates throughput value for given bytes number and time step index
   * @param {number} statValue transfered bytes
   * @param {number} statTimeIndex time step index
   * @returns {number} average throughput in bytes per second
   */
  _scaleStatValue(statValue, statTimeIndex) {
    const {
      _timePeriod,
      _statsTimestamp,
      _transferStartTime,
    } = this.getProperties(
      '_timePeriod',
      '_statsTimestamp',
      '_transferStartTime'
    );

    const transferTime = _statsTimestamp - _transferStartTime;
    const timePeriodInSec =
      moment.duration(_timePeriod[0], _timePeriod[1]).asSeconds();
    const timeSinceLastStat = transferTime - timePeriodInSec * statTimeIndex;
    const timeDivider = Math.min(Math.max(1, timeSinceLastStat), timePeriodInSec);
    return statValue / timeDivider;
  }
});
