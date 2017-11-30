/**
 * A stacked line chart component for visualizing transfer throughput history.
 * 
 * @module components/transfers/transfer-chart
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import additionalXLabel from 'op-worker-gui/utils/chartist/additional-x-label';
import shortHorizontalGrid from 'op-worker-gui/utils/chartist/short-horizontal-grid';
import tooltip from 'op-worker-gui/utils/chartist/tooltip';
import centerLineChart from 'op-worker-gui/utils/chartist/center-line-chart';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import axisLabels from 'op-worker-gui/utils/chartist/axis-labels';
import stackedLineMask from 'op-worker-gui/utils/chartist/stacked-line-mask';
import TransferTimeStatUpdater from 'op-worker-gui/utils/transfer-time-stat-updater';
import customCss from 'op-worker-gui/utils/chartist/custom-css';

const {
  Component,
  computed,
  get,
  inject: {
    service,
  },
} = Ember;

const I18N_PREFIX = 'components.transfers.transferChart.';
// const UNITS = ['minute', 'hour', 'day', 'month'];

export default Component.extend({
  classNames: ['transfers-transfer-chart'],
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
  _lastUpdateTime: computed('_timeStatForUnit.timestamp', 'transfer.isCurrent', function () {
    const {
      _timeStatForUnit,
      transfer,
    } = this.getProperties('_timeStatForUnit', 'transfer');
    if (transfer.get('isCurrent')) {
      const date = get(_timeStatForUnit, 'timestamp');
      return date ? date : moment(new Date()).unix();
    } else {
      return transfer.get('finishTime');
    }
  }),
  
  /**
   * One of `minute`, `hour`, `day`, `month`.
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

  _updaterEnabled: true,
  
  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _stats: computed.reads('_timeStatForUnit.content.stats'),

  /**
   * Last stat timestamp
   * @type {Ember.ComputedProperty<number>}
   */
  _statsTimestamp: computed('_timeStatForUnit.content.timestamp', 'transfer.isCurrent', function () {
    const transfer = this.get('transfer');
    if (transfer.get('isCurrent')) {
      return this.get('_timeStatForUnit.content.timestamp');
    } else {
      return transfer.get('finishTime');
    }
  }),

  /**
   * True if data for chart is loaded
   * @type {boolean}
   */
  _statsLoaded: computed('_timeStatForUnit.content.stats', function() {
    return this.get('_timeStatForUnit.isLoaded');
  }),
  
  /**
   * @type {Ember.ComputedProperty<number>}
   */
  _transferStartTime: computed.reads('transfer.startTime'),

  /**
   * Expected stats number (number of chart points).
   * @type {Ember.ComputedProperty<number>}
   */
  _expectedStatsNumber: computed('timeUnit', function () {
    return this._getExpectedStatsNumberForUnit(this.get('timeUnit'));
  }),

  /**
   * A number of stats, that should be considered as a single chart value
   * @type {Ember.ComputedProperty<number>}
   */
  _statsUnitsPerChartValue: computed('_expectedStatsNumber', function () {
    const {
      _expectedStatsNumber,
      timeUnit,
    } = this.getProperties('_expectedStatsNumber', 'timeUnit');
    let statsPerUnit;
    switch (timeUnit) {
      case 'month':
        statsPerUnit = 30;
        break;
      case 'day':
        statsPerUnit = 24;
        break;
      case 'hour':
        statsPerUnit = 60;
        break;
      default:
      case 'minute':
        statsPerUnit = 12;
    }
    return statsPerUnit / _expectedStatsNumber;
  }),

  // FIXME: this should be auto updated
  /**
   * Object with stats for specified time unit.
   * @type {Ember.ComputedProperty<TransferTimeStat>}
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
   * Object that sets for each time unit if it should be visible to user
   * @type {Ember.ComputedProperty<Ember.Object>}
   */
  _unitVisibility: computed('_transferStartTime', '_lastUpdateTime', function () {
    const {
      _transferStartTime,
      _lastUpdateTime,
    } = this.getProperties('_transferStartTime', '_lastUpdateTime');
    const transferTime = _lastUpdateTime - _transferStartTime;
    const result = Ember.Object.create({
      minute: true,
      hour: true,
    });
    const compareUnit = ['hour', 'day'];
    ['day', 'month'].forEach((unit, index) => {
      const period = this._getTimePeriodForUnit(compareUnit[index]);
      const periodInSeconds = moment.duration(period[0], period[1]).asSeconds();
      result.set(unit, transferTime > periodInSeconds);
    });
    return result;
  }),
  
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
      _expectedStatsNumber,
    } = this.getProperties(
      '_stats',
      '_statsUnitsPerChartValue',
      '_sortedProvidersIds',
      '_expectedStatsNumber'
    );
    const inputStatsValuesNumber = _expectedStatsNumber * _statsUnitsPerChartValue;
    const statsValues = [];

    _sortedProvidersIds.forEach(key => {
      let values = _stats[key];
      if (values.length < inputStatsValuesNumber) {
        values = values.concat(_.range(inputStatsValuesNumber - values.length).map(() => 0));
      }
      const scaledValues = [];
      for (let i = 0; i < values.length; i += _statsUnitsPerChartValue) {
        scaledValues.push(this._scaleStatValue(
          values.slice(i, i + _statsUnitsPerChartValue),
          scaledValues.length
        ));
      }
      statsValues.push(scaledValues.reverse());
    });
    return statsValues;
  }),

  /**
   * Chart time period
   * @type {Ember.ComputedProperty<Array<any>>}
   */
  _timePeriod: computed('_expectedStatsNumber', function () {
    return this._getTimePeriodForUnit(this.get('timeUnit'));
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
      case 'month':
        return 'DD/MM';
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
        left: 60,
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
        stackedLineMask(),
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
      _expectedStatsNumber,
    } = this.getProperties(
      '_statsValues',
      '_chartValues',
      '_sortedProvidersIds',
      'providersColors',
      'providers',
      '_expectedStatsNumber'
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
    const valuesSumArray = _.times(_expectedStatsNumber, _.constant(0));
    _statsValues.slice(0).reverse().forEach((providerValues, index) => {
      providerValues.forEach((value, index2) => valuesSumArray[index2] += value);
      _chartValues[_chartValues.length - index - 1].push(...(valuesSumArray));
    });
    // creating tooltips
    const tooltipElements = _.range(_expectedStatsNumber).map((index) => {
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
      return _.times(_expectedStatsNumber, _.constant({
        line: {
          stroke: color,
        },
        point: {
          stroke: color,
        },
        area: {
          fill: color,
        }
      }));
    });
    // creating chart data object
    return {
      labels: _.range(1, _expectedStatsNumber + 1).reverse()
        .map(n => this._getChartLabel(n)),
      series: _chartValues.map((providerValues) => ({
        data: providerValues,
        tooltipElements,
      })),
      lastLabel: this._getChartLabel(0),
      customCss,
    };
  }),
  
  init() {
  this._super(...arguments);
    this.set('_chartValues', []);
    const isCurrent = this.get('transfer.isCurrent');
    const gettingStats = this.get('_timeStatForUnit');
    
    if (isCurrent) {
      console.log('transfer-chart: creating updater');
      gettingStats.then(timeStat => {
        const updater = TransferTimeStatUpdater.create({
          isEnabled: this.get('_updaterEnabled'),
          timeStat,
        });
        this.set('updater', updater);
      }); 
    }
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
    return moment.unix(_lastUpdateTime)
      .subtract(offset * _timePeriod[0], _timePeriod[1])
      .format(_timeFormat);
  },

  /**
   * Calculates throughput value for given bytes number and time step index
   * @param {Array<number>} statValue transfered bytes/s for chart value
   * @param {number} statTimeIndex time step index
   * @returns {number} average throughput in bytes per second
   */
  _scaleStatValue(statValues, statTimeIndex) {
    const {
      _timePeriod,
      _statsTimestamp,
      _transferStartTime,
    } = this.getProperties(
      '_timePeriod',
      '_statsTimestamp',
      '_transferStartTime'
    );

    const transferTime = _statsTimestamp - _transferStartTime + 1;
    const timePeriodInSec =
      moment.duration(_timePeriod[0], _timePeriod[1]).asSeconds();
    const timeSinceLastStat = transferTime - timePeriodInSec * statTimeIndex;
    const chartValueTime = Math.min(Math.max(1, timeSinceLastStat), timePeriodInSec);
    const oneStatTime = timePeriodInSec / statValues.length;
    let bytes = 0;
    const completeStatsNumber = Math.floor(chartValueTime / oneStatTime);
    for (let i = 0; i < completeStatsNumber; i++) {
      bytes += statValues[i] * oneStatTime;
    }
    const timeRemainder = chartValueTime % oneStatTime;
    if (timeRemainder) {
      bytes += statValues[completeStatsNumber] * timeRemainder;
    }
    return bytes / chartValueTime;
  },

  /**
   * Chart time period
   * @returns {Array<any>}
   */
  _getTimePeriodForUnit(unit) {
    const _expectedStatsNumber = this._getExpectedStatsNumberForUnit(unit);
    switch (unit) {
      case 'month':
        return [30 / _expectedStatsNumber, 'days'];
      case 'day':
        return [24 / _expectedStatsNumber, 'hours'];
      case 'hour':
        return [60 / _expectedStatsNumber, 'minutes'];  
      default:
        return [60 / _expectedStatsNumber, 'seconds'];      
    }
  },

  /**
   * Expected stats number (number of chart points).
   * @returns {Ember.ComputedProperty<number>}
   */
  _getExpectedStatsNumberForUnit(unit) {
    switch (unit) {
      case 'month':
        return 15;
      default:
        return 12;
    }
  },

  /**
   * Return preffered time unit for displaying transfer
   * @returns {string}
   */
  // _getPrefferedUnit() {
  //   const {
  //     _transferStartTime,
  //     _lastUpdateTime,
  //   } = this.getProperties('_transferStartTime', '_lastUpdateTime');
  //   const transferTime = _lastUpdateTime - _transferStartTime;
  //   let prefferedUnit;
  //   UNITS.slice(0).forEach(unit => {
  //     if (!prefferedUnit) {
  //       const period = this._getTimePeriodForUnit(unit);
  //       const periodInSeconds = moment.duration(_period[0], _period[1]).asSeconds();
  //       if (transferTime > periodInSeconds) {
  //         prefferedUnit = unit;
  //       }
  //     }
  //   });
  //   return prefferedUnit || 'minute';
  // }
});
