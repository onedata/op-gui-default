/**
 * A stacked line chart component for visualizing transfer throughput history.
 * 
 * @module components/transfers/transfer-chart
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */
 
import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import axisLabels from 'op-worker-gui/utils/chartist/axis-labels';
import stackedLineMask from 'op-worker-gui/utils/chartist/stacked-line-mask';
import TransferTimeStatUpdater from 'op-worker-gui/utils/transfer-time-stat-updater';
import customCss from 'op-worker-gui/utils/chartist/custom-css';
import centerXLabels from 'op-worker-gui/utils/chartist/center-x-labels';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';
import eventListener from 'op-worker-gui/utils/chartist/event-listener';
import ChartistValuesLine from 'op-worker-gui/mixins/components/chartist-values-line';
import ChartistTooltip from 'op-worker-gui/mixins/components/chartist-tooltip';

const {
  Component,
  computed,
  get,
  RSVP: { Promise },
  observer,
  inject: {
    service,
  },
  String: {
    htmlSafe
  },
} = Ember;

/* global Chartist */

const I18N_PREFIX = 'components.transfers.transferChart.';

export default Component.extend(ChartistValuesLine, ChartistTooltip, {
  classNames: ['transfers-transfer-chart'],
  i18n: service(),
  
  /**
   * @virtual
   * @type {Transfer}
   */
  transfer: Object.freeze({}),

  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,

  /**
   * One of `minute`, `hour`, `day`, `month`.
   * @type {string}
   * @virtual
   */
  timeUnit: 'minute',

  /**
   * Preffered init time unit
   * @type {string}
   */
  prefferedUnit: undefined,

  /**
   * Can by used by stateless transfers like on-the-fly
   * @type {boolean}
   */
  ignoreTransferState: false,

  /**
   * Colors used to color each providers' series
   * @virtual
   * @type {Ember.ComputedProperty<Object>}
   */
  providersColors: Object.freeze({}),

  /**
   * @override
   * @type {string}
   */
  chartTooltipSelector: '.ct-tooltip',

  /**
   * @override
   * @type {string}
   */
  chartTooltipVerticalAlign: 'top',
  
  /**
   * Array of actual chart values.
   * @type {Array<Array<object>>}
   */
  _chartValues: undefined,

  /**
   * Initialized when stat record is available (after init)
   * @type {TransferTimeStatUpdater}
   */
  updater: undefined,
  
  _updaterEnabled: true,

  /**
   * Set this value if want to override updater.fetchError (eg. if updater cannot be
   * created)
   * @type {string}
   */
  _statsError: undefined,

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  _transferIsScheduled: computed.equal('transfer.status', 'scheduled'),
  
  /**
   * True if data for chart is loaded
   * @type {boolean}
   */
  _statsLoaded: computed(
    '_timeStatForUnit.isFulfilled',
    'ignoreTransferState',
    'transfer.currentStat.isFulfilled',
    function () {
      const result = this.get('_timeStatForUnit.isFulfilled');
      if (this.get('ignoreTransferState')) {
        return result;
      }
      return result && this.get('transfer.currentStat.isFulfilled');
    }
  ),

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  _showUnitButtons: computed('_transferIsScheduled', '_isWaitingForStats', function () {
    const {
      _transferIsScheduled,
      _isWaitingForStats,
    } = this.getProperties('_transferIsScheduled', '_statsLoaded', '_isWaitingForStats');
    return !_transferIsScheduled && !_isWaitingForStats;
  }),

  /**
   * @type {Ember.ComputedProperty<string|null>}
   */
  statsError: computed('updater.fetchError', '_statsError', function () {
    return this.get('updater.fetchError') || this.get('_statsError');
  }),
  
  /**
   * Proxy object that resolves with stats for specified time unit.
   * @type {Ember.ComputedProperty<PromiseObject<TransferTimeStat>>}
   */
  _timeStatForUnit: computed('transfer', 'timeUnit', function () {
    const {
      transfer,
      timeUnit,
    } = this.getProperties('transfer', 'timeUnit');
    if (get(transfer, 'isLoaded')) {
      return get(transfer, `${timeUnit}Stat`);
    } else {
      const promise = new Promise((resolve, reject) => {
        transfer.on('didLoad', () => {
          get(transfer, `${timeUnit}Stat`)
            .then(resolve)
            .catch(reject);
        });
      });
      return PromiseObject.create({ promise });
    }
  }),

  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _stats: computed.reads('_timeStatForUnit.stats'),

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  _noStatsForUnit: computed('_statsLoaded', '_stats', function () {
    const {
      _statsLoaded,
      _stats,
    } = this.getProperties('_statsLoaded', '_stats');
    return _statsLoaded && Object.keys(_stats).length === 0;
  }),

  /**
   * Last update time (async -> _timeStatForUnit)
   * @type {Ember.ComputedProperty<Date>}
   */
  _transferLastUpdateTime: computed.reads('_timeStatForUnit.timestamp'),
  
  /**
   * @type {Ember.ComputedProperty<number>}
   */
  _transferStartTime: computed('transfer.startTime', function () {
    return this.get('transfer.startTime') || 0;
  }),

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  _isWaitingForStats: computed(
    '_statsLoaded',
    'transfer.isCurrent',
    '_transferLastUpdateTime',
    '_transferStartTime',
    function () {
      const {
        _statsLoaded,
        transfer,
        _transferLastUpdateTime,
        _transferStartTime,
      } = this.getProperties(
        '_statsLoaded',
        'transfer',
        '_transferLastUpdateTime',
        '_transferStartTime'
      );
      if (!_statsLoaded || !transfer.get('isCurrent')) {
        return false;
      } else {
        return _transferLastUpdateTime - _transferStartTime < 30;
      }
    }
  ),

  /**
   * Expected stats number (number of chart points).
   * @type {Ember.ComputedProperty<number>}
   */
  _expectedStatsNumber: computed('timeUnit', function () {
    return this._getExpectedStatsNumberForUnit(this.get('timeUnit'));
  }),

  /**
   * Sorted provider ids.
   * @type {Ember.ComputedProperty<Array<string>|undefined>}
   */
  _sortedProvidersIds: computed('_stats', function () {
    const _stats = this.get('_stats');
    if (_stats) {
      return Object.keys(_stats).sort();
    }
  }),

  /**
   * Chart time period
   * @type {Ember.ComputedProperty<number>}
   */
  _timePeriod: computed('timeUnit', function () {
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
   * Number of stats, that will be grouped under the same x axis label
   * @returns {Ember.ComputedProperty<number>}
   */
  _statsNumberPerLabel: computed('timeUnit', function() {
    switch (this.get('timeUnit')) {
      case 'month':
        return 3;
      case 'day':
        return 4;
      case 'hour':
        return 6;
      default:
        return 2;
    }
  }),

  /**
   * Object that sets for each time unit if it should be visible to user or not
   * @type {Ember.ComputedProperty<Ember.Object>}
   */
  _unitVisibility: computed('_transferStartTime', '_transferLastUpdateTime', function () {
    const {
      _transferStartTime,
      _transferLastUpdateTime,
    } = this.getProperties('_transferStartTime', '_transferLastUpdateTime');
    const transferTime = _transferLastUpdateTime - _transferStartTime;
    const result = Ember.Object.create({
      minute: true,
      hour: true,
    });
    const compareUnit = ['hour', 'day'];
    ['day', 'month'].forEach((unit, index) => {
      const period = this._getTimePeriodForUnit(compareUnit[index]);
      result.set(unit, transferTime > period);
    });
    return result;
  }),
  
  /**
   * Stats values (points x,y), that will be used to as a source for chart values.
   * (async -> _stats)
   * @type {Ember.ComputedProperty<Array<number>|undefined>}
   */
  _statsValues: computed('_stats', '_sortedProvidersIds', '_expectedStatsNumber', function () {
    const {
      _stats,
      _sortedProvidersIds,
      _expectedStatsNumber,
    } = this.getProperties(
      '_stats',
      '_sortedProvidersIds',
      '_expectedStatsNumber'
    );
    if (_sortedProvidersIds) {
      return _sortedProvidersIds.map(key => {
        let values = _stats[key];
        if (values.length < _expectedStatsNumber) {
          values = values.concat(_.times(_expectedStatsNumber - values.length, _.constant(null)));
        }
        return this._scaleStatValue(values);
      }); 
    }
  }),

  /**
   * Minimal x-axis value for chart.
   * @type {Ember.ComputedProperty<number>}
   */
  _chartXLow: computed('_timePeriod', '_transferLastUpdateTime', function () {
    const {
      _timePeriod,
      _transferLastUpdateTime,
      _expectedStatsNumber,
    } = this.getProperties(
      '_timePeriod',
      '_transferLastUpdateTime',
      '_expectedStatsNumber'
    );
    return _transferLastUpdateTime - _timePeriod * _expectedStatsNumber + 1;
  }),

  /**
   * Axis x labels
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  _chartXTicks: computed('_timePeriod', '_statsNumberPerLabel', '_transferLastUpdateTime', function () {
    const {
      _statsNumberPerLabel,
      _timePeriod,
      _transferLastUpdateTime,
      _chartXLow,
    } = this.getProperties(
      '_statsNumberPerLabel',
      '_timePeriod',
      '_transferLastUpdateTime',
      '_chartXLow'
    );
    let x = _transferLastUpdateTime - (_transferLastUpdateTime % _timePeriod);
    const ticks = [];
    while (x > _chartXLow) {
      ticks.push(x);
      x -= _timePeriod;
    }
    return ticks.filter((value, index) => index % _statsNumberPerLabel === 0);
  }),

  /**
   * Maximum stats sum in all time slots
   * @type {Ember.ComputedProperty<number>}
   */
  _chartYMax: computed('_stats', function () {
    const _stats = this.get('_stats');
    const arrays = _.values(_stats);
    if (!arrays.length) {
      return 0;
    }
    let maxSum = 0;
    _.range(arrays[0].length).forEach(i => {
      const sum = _.sum(arrays.map(ar => ar[i] || 0));
      if (sum > maxSum) {
        maxSum = sum;
      }
    });
    return Math.max(maxSum, 8);
  }),

  /**
   * Chart ticks for Y axis
   * @type {Ember.ComputedProperty<number>}
   */
  _chartYTicks: computed('_chartYMax', function () {
    const _chartYMax = this.get('_chartYMax');
    const numberOfTicks = 4;
    const delta = _chartYMax / (numberOfTicks - 1);
    return _.range(numberOfTicks).map(i => delta * i);
  }),

  /**
   * Chartist settings
   * @type {Object}
   */
  _chartOptions: computed('_chartXTicks', '_chartYTicks', function() {
    const {
      i18n,
      _chartXTicks,
      _chartXLow,
      _chartYMax,
      _chartYTicks,
    } = this.getProperties(
      'i18n',
      '_chartXTicks',
      '_chartXLow',
      '_chartYMax',
      '_chartYTicks'
    );
    return {
      axisX: {
        type: Chartist.FixedScaleAxis,
        ticks: _chartXTicks,
        labelInterpolationFnc: value => this._formatStatTime(value),
        showGrid: false,
        low: _chartXLow,
      },
      axisY: {
        low: 0,
        high: _chartYMax,
        type: Chartist.FixedScaleAxis,
        labelInterpolationFnc: value => bytesToString(value, { format: 'bit' }) + 'ps',
        ticks: _chartYTicks,
      },
      low: 0,
      showArea: true,
      fullWidth: true,
      chartPadding: {
        top: 30,
        bottom: 30,
        left: 60,
        right: 50,
      },
      plugins: [
        axisLabels({
          xLabel: i18n.t(I18N_PREFIX + 'time'),
          yLabel: i18n.t(I18N_PREFIX + 'throughput'),
        }),
        stackedLineMask(),
        customCss({
          filterBySeriesIndex: true,
        }),
        centerXLabels(),
        eventListener({
          eventHandler: (eventData) => this._chartEventHandler(eventData),
        }),
      ],
    };
  }),

  /**
   * Data for chartist (async -> _statsValues)
   * @type {Ember.ComputedProperty<Object|undefined>}
   */
  _chartData: computed(
    '_statsValues',
    '_transferStartTime',
    '_sortedProvidersIds',
    'providersColors',
    'providers.@each.name',
    '_expectedStatsNumber',
    function () {
      const {
        _statsValues,
        _chartValues,
        _sortedProvidersIds,
        providersColors,
        _expectedStatsNumber,
        _transferStartTime,
      } = this.getProperties(
        '_statsValues',
        '_chartValues',
        '_sortedProvidersIds',
        'providersColors',
        '_expectedStatsNumber',
        '_transferStartTime'
      );
      if (_statsValues) {
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
        const valuesSumArray = _.range(_expectedStatsNumber + 2).map(() => ({ x: 0, y: 0 }));
        for (let i = _statsValues.length - 1; i >= 0; i--) {
          /* jshint loopfunc: true */
          const providerValues = _statsValues[i];
          providerValues.forEach((value, valueIndex) => {
            valuesSumArray[valueIndex].y += value.y;
            valuesSumArray[valueIndex].x = value.x;
          });
          _chartValues[i]
            .push(..._.cloneDeep(
              valuesSumArray.filter(({ x }) => x >= _transferStartTime)
            ));
          if (valuesSumArray[_expectedStatsNumber + 1].x === 0) {
            _chartValues[i].pop();
          }
        }
        // setting colors
        const customCss = _sortedProvidersIds.map((providerId) => {
          const color = providersColors[providerId] || providersColors['unknown'];
          return _.times(_expectedStatsNumber + 2, _.constant({
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
          series: _chartValues.map((providerValues) => ({
            data: providerValues,
          })),
          customCss,
        }; 
      }
    }
  ),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  _tooltipHeader: computed(
    '_statsValues',
    'chartTooltipHoveredColumn',
    function () {
      const chartTooltipHoveredColumn = this.get('chartTooltipHoveredColumn');
      const chartLabels = this._getChartLabels();
      const startTime = chartLabels[chartTooltipHoveredColumn];
      const endTime = chartLabels[chartTooltipHoveredColumn - 1];
      if (chartTooltipHoveredColumn === 0 || startTime === endTime) {
        return startTime;
      } else {
        return endTime + ' - ' + startTime;
      }
    }
  ),

  /**
   * @type {Ember.ComputedProperty<Array<object>>}
   */
  _tooltipProviders: computed(
    '_sortedProvidersIds',
    'providersColors',
    'providers',
    '_stats',
    'chartTooltipHoveredColumn',
    function () {
      const {
        _sortedProvidersIds,
        _stats,
        providersColors,
        providers,
        chartTooltipHoveredColumn,
        chartTooltipColumnsNumber,
      } = this.getProperties(
        '_sortedProvidersIds',
        '_stats',
        'providersColors',
        'providers',
        'chartTooltipHoveredColumn',
        'chartTooltipColumnsNumber'
      );
      const result = [];
      _sortedProvidersIds.forEach(providerId => {
        const providerStats = _stats[providerId];
        const index = chartTooltipColumnsNumber - chartTooltipHoveredColumn - 1;
        if (index < 0 || !providerStats[index]) {
          return;
        }
        const provider =
          providers.filter((provider) => get(provider, 'id') === providerId)[0] || {};
        const providerName = get(provider, 'name') || providerId;
        result.push({
          name: providerName,
          valueNumber: providerStats[index],
          value: bytesToString(providerStats[index], { format: 'bit' }) + 'ps',
          boxStyle: htmlSafe(
            'background-color: ' +
            providersColors[providerId] || providersColors['unknown']
          ),
        });
      });
      return result;
    }
  ),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  _tooltipSum: computed('_tooltipProviders', function () {
    const bytes = _.sum(this.get('_tooltipProviders').map(p => p.valueNumber));
    return bytesToString(bytes, { format: 'bit' }) + 'ps';
  }),
  
  changeUpdaterUnit: observer(
    'updater',
    '_timeStatForUnit.content',
    function observeChangeUpdaterUnit() {
      const timeStat = this.get('_timeStatForUnit.content');
      const updater = this.get('updater');
      if (updater && timeStat && timeStat !== this.get('updater.timeStat')) {
        this.set('updater.timeStat', timeStat);
      }
    }
  ),

  init() {
    this._super(...arguments);
    this.set('_chartValues', []);
    const transfer = this.get('transfer');
    
    if (get(transfer, 'isLoaded')) {
      this._createTimeStatsUpdater();
    } else {
      transfer.on('didLoad', () => {
        this._createTimeStatsUpdater();
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

  _createTimeStatsUpdater() {
    const {
      transfer,
      _timeStatForUnit,
      ignoreTransferState,
      _updaterEnabled
    } = this.getProperties(
      'transfer',
      '_timeStatForUnit',
      'ignoreTransferState',
      '_updaterEnabled'
    );
    const isCurrent = get(transfer, 'isCurrent');
 
    console.log('transfer-chart: creating updater');
    _timeStatForUnit
      .then(timeStat => {
        this.set('_statsError', null);
        if (!isCurrent) {
          this.set('timeUnit', this._getPrefferedUnit());
        }
        const updater = TransferTimeStatUpdater.create({
          isEnabled: ignoreTransferState ?
            _updaterEnabled :
            isCurrent && _updaterEnabled,
          timeStat,
        });
        if (!isCurrent) {
          updater.fetch();
        }
        this.set('updater', updater);
      })
      .catch(error => {
        this.set('_statsError', error);
      });
  },
  
  /**
   * Calculates throughput value for given bytes number and time step index
   * @param {Array<number>} statValue transfered bytes/s for chart value
   * @param {number} statTimeIndex time step index
   * @returns {number} average throughput in bytes per second
   */
  _scaleStatValue(statValues) {
    const {
      _timePeriod,
      _transferLastUpdateTime,
      _transferStartTime,
      _expectedStatsNumber,
    } = this.getProperties(
      '_timePeriod',
      '_transferLastUpdateTime',
      '_transferStartTime',
      '_expectedStatsNumber'
    );
    let x = _transferLastUpdateTime + 1;
    const scaledStats = [];
    statValues = statValues.filter(y => y !== null);
    for (let i = 0; i < statValues.length; i++) {
      scaledStats.push({ x, y: statValues[i] });
      const timeDelta = x % _timePeriod === 0 ? _timePeriod : x % _timePeriod;
      const newX = Math.max(
        x - timeDelta,
        _transferStartTime,
        _transferLastUpdateTime - _timePeriod * _expectedStatsNumber
      );
      if (newX === x) {
        break;
      } else {
        x = newX;
      }
    }
    return scaledStats.reverse();
  },

  /**
   * Chart time period
   * @returns {Array<any>}
   */
  _getTimePeriodForUnit(unit) {
    const _expectedStatsNumber = this._getExpectedStatsNumberForUnit(unit);
    switch (unit) {
      case 'month':
        // 30 days
        return 2592000 / _expectedStatsNumber;
      case 'day':
        return 86400 / _expectedStatsNumber;
      case 'hour':
        return 3600 / _expectedStatsNumber;  
      default:
        return 60 / _expectedStatsNumber;      
    }
  },

  /**
   * Expected stats number (number of chart points).
   * @returns {Ember.ComputedProperty<number>}
   */
  _getExpectedStatsNumberForUnit(unit) {
    switch (unit) {
      case 'month':
        return 30;
      case 'day':
        return 24;
      case 'hour':
        return 60;
      default:
        return 12;
    }
  },

  _getChartLabels() {
    const _statsValues = this.get('_statsValues');
    return _statsValues.length ? _statsValues[0].map(({x}) => this._formatStatTime(x)) : [];
  },

  _formatStatTime(time) {
    return moment.unix(time).format(this.get('_timeFormat'));
  },

  /**
   * Returns preffered time unit for displaying transfer stats
   * @returns {string}
   */
  _getPrefferedUnit() {
    let {
      _transferStartTime,
      _transferLastUpdateTime,
      prefferedUnit
    } = this.getProperties(
      '_transferStartTime',
      '_transferLastUpdateTime',
      'prefferedUnit'
    );
    if (prefferedUnit) {
      return prefferedUnit;
    }
    const transferTime = _transferLastUpdateTime - _transferStartTime;
    ['minute', 'hour', 'day'].forEach(unit => {
      if (!prefferedUnit) {
        const timeWindow = this._getTimePeriodForUnit(unit) *
          this._getExpectedStatsNumberForUnit(unit);
        if (transferTime <= timeWindow) {
          prefferedUnit = unit;
        }
      }
    });
    return prefferedUnit || 'month';
  },

  /**
   * Attaches all needed handlers to the chart
   * @param {object} param event data
   */
  _chartEventHandler(eventData) {
    this.addChartValuesLine(eventData);
    this.addChartTooltip(eventData);
  },
});
