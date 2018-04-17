/**
 * A stacked line chart component for visualizing all transfers throughput history.
 * 
 * @module components/transfers/throughput-distribution
 * @author Michal Borzecki, Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */
 
import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import stackedLineMask from 'op-worker-gui/utils/chartist/stacked-line-mask';
import TransferTimeStatUpdater from 'op-worker-gui/utils/transfer-time-stat-updater';
import customCss from 'op-worker-gui/utils/chartist/custom-css';
import centerXLabels from 'op-worker-gui/utils/chartist/center-x-labels';
import axisLabels from 'op-worker-gui/utils/chartist/axis-labels';
import eventListener from 'op-worker-gui/utils/chartist/event-listener';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';
import $ from 'jquery';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import computedPipe from 'ember-cli-onedata-common/utils/ember/computed-pipe';
import ChartistValuesLine from 'op-worker-gui/mixins/components/chartist-values-line';
import ChartistTooltip from 'op-worker-gui/mixins/components/chartist-tooltip';

const {
  Component,
  computed,
  get,
  observer,
  inject: {
    service,
  },
  String: {
    htmlSafe
  },
} = Ember;

const I18N_PREFIX = 'components.transfers.throughputDistribution.';

const subunitSuffix = {
  minute: 's',
  hour: 'm',
  day: 'h',
  month: 'd',
};

const subunit = {
  minute: 'second',
  hour: 'minute',
  day: 'hour',
  month: 'day',
};

export default Component.extend(ChartistValuesLine, ChartistTooltip, {
  classNames: ['transfers-throughput-distribution'],
  i18n: service(),
  store: service(),

  /**
   * @type {Space}
   */
  space: undefined,

  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,

  /**
   * Id of provider, which stats should be visualized
   * @type {string}
   */
  transferStatProviderId: null,

  /**
   * Possible values: onTheFly, job, all.
   * @type {string}
   */
  transferType: 'all',

  /**
   * One of `minute`, `hour`, `day`, `month`.
   * @type {string}
   */
  timeUnit: 'minute',

  /**
   * Colors used to color each providers' series
   * @virtual
   * @type {Object}
   */
  providersColors: undefined,

  /**
   * Selects stats provider
   * @virtual
   * @type {function}
   */
  selectTransferStatProvider: () => {},

  /**
   * @override
   * @type {string}
   */
  chartTooltipSelector: '.ct-tooltip',
  
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
  
  /**
   * @type {boolean}
   */
  _updaterEnabled: true,

  /**
   * Set this value if want to override updater.fetchError (eg. if updater cannot be
   * created)
   * @type {string}
   */
  _statsError: undefined,
  
  /**
   * @type {Ember.ComputedProperty<object>}
   */
  _providersNames: computed('providers.@each.name', function () {
    const providers = this.get('providers');
    const names = {};
    providers.forEach(provider => {
      const id = get(provider, 'id');
      names[id] = get(provider, 'name') || id;
    });
    return names;
  }),
  
  /**
   * True if data for chart is loaded
   * @type {boolean}
   */
  _statsLoaded: computed.reads('_timeStatForUnit.isFulfilled'),

  /**
   * @type {Ember.ComputedProperty<string|null>}
   */
  statsError: computed('updater.fetchError', '_statsError', function () {
    return this.get('updater.fetchError') || this.get('_statsError');
  }),
  
  /**
   * Proxy object that resolves with stats for specified type and time unit.
   * @type {Ember.ComputedProperty<PromiseObject<SpaceTransferTimeStat>>}
   */
  _timeStatForUnit: computed(
    'space',
    'timeUnit',
    'transferStatProviderId',
    'transferType',
    function () {
      const {
        space,
        timeUnit,
        transferType,
        // transferStatProviderId,
      } = this.getProperties(
        'space',
        'timeUnit',
        'transferType',
        'transferStatProviderId'
      );
      // uncomment to enable per-provider-stats fetch
      // const transferProviderStat = get(space, 'transferProviderStat');
      // const providerStats = transferStatProviderId &&
      //   get(transferProviderStat, transferStatProviderId);
      const unitProp = `${timeUnit}Stat`;
      let promise;
      // if (providerStats) {
      //   const typeProp = `${transferType}Stat`;
      //   promise = this.store
      //     .findRecord('space-transfer-stat', get(providerStats, typeProp))
      //     .then(typeStats => get(typeStats, unitProp))
      // } else {
        const typeProp = `transfer${_.upperFirst(transferType)}Stat`;
        promise = get(space, typeProp).then(typeStats => get(typeStats, unitProp));
      // }
      return PromiseObject.create({ promise });
    }
  ),

  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _statsIn: computedPipe('_timeStatForUnit.statsIn', '_removeZeroStats'),

  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _statsOut: computedPipe('_timeStatForUnit.statsOut', '_removeZeroStats'),

  /**
   * Last update time (async -> _timeStatForUnit)
   * @type {Ember.ComputedProperty<Date>}
   */
  _statEndTime: computed.reads('_timeStatForUnit.timestamp'),

  /**
   * Maximum input stats sum in all time slots
   * @type {Ember.ComputedProperty<number>}
   */
  _maxStatsInSum: computedPipe('_statsIn', '_calculateStatsMaxSum'),

  /**
   * Maximum output stats sum in all time slots
   * @type {Ember.ComputedProperty<number>}
   */
  _maxStatsOutSum: computedPipe('_statsOut', '_calculateStatsMaxSum'),

  /**
   * Expected stats number (number of chart points).
   * @type {Ember.ComputedProperty<number>}
   */
  _expectedStatsNumber: computedPipe('timeUnit','_getExpectedStatsNumberForUnit'),

  /**
   * Sorted input provider ids.
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  _sortedInProvidersIds: computed('_statsIn', function () {
    return Object.keys(this.get('_statsIn')).sort();
  }),

  /**
   * Sorted output provider ids.
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  _sortedOutProvidersIds: computed('_statsOut', function () {
    return Object.keys(this.get('_statsOut')).sort();
  }),

  /**
   * Sorted provider ids (in order [...in, ...out]).
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  _sortedProvidersIds: computed('_sortedInProvidersIds', '_sortedOutProvidersIds', function () {
    const {
      _sortedInProvidersIds,
      _sortedOutProvidersIds,
    } = this.getProperties('_sortedInProvidersIds', '_sortedOutProvidersIds');
    return _sortedInProvidersIds.concat(_sortedOutProvidersIds);
  }),

  /**
   * Chart time period
   * @type {Ember.ComputedProperty<number>}
   */
  _timePeriod: computedPipe('timeUnit', '_getTimePeriodForUnit'),
  
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
        return 1;
    }
  }),
  
  /**
   * Stats values (points x,y), that will be used as a source for chart values.
   * @type {Ember.ComputedProperty<Array<number>|undefined>}
   */
  _statsValues: computed('_statsIn', '_statsOut', '_sortedProvidersIds', '_expectedStatsNumber', function () {
    const {
      _statsIn,
      _statsOut,
      _sortedProvidersIds,
      _expectedStatsNumber,
    } = this.getProperties(
      '_statsIn',
      '_statsOut',
      '_sortedProvidersIds',
      '_expectedStatsNumber'
    );
    if (_sortedProvidersIds) {
      const inProvidersNo = Object.keys(_statsIn).length;
      return _.range(_sortedProvidersIds.length).map(index => {
        const providerId = _sortedProvidersIds[index];
        let values = index < inProvidersNo ? _statsIn[providerId] : _statsOut[providerId];
        if (values.length < _expectedStatsNumber + 2) {
          values = values.concat(_.times(_expectedStatsNumber + 2 - values.length, _.constant(0)));
        }
        if (index >= inProvidersNo) {
          values = values.map(v => typeof v === 'number' ? -v : v);
        }
        return this._scaleStatValue(values);
      });
    }
  }),

  /**
   * Axis x labels
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  _chartXTicks: computed('_timePeriod', '_statsNumberPerLabel', '_expectedStatsNumber', function () {
    const {
      _statsNumberPerLabel,
      _timePeriod,
      _expectedStatsNumber,
    } = this.getProperties(
      '_statsNumberPerLabel',
      '_timePeriod',
      '_expectedStatsNumber'
    );
    return _.range(_expectedStatsNumber)
      .filter((index) => index % _statsNumberPerLabel === 0)
      .map((i) => -_timePeriod * i);
  }),

  /**
   * @type {Ember.ComputedProperty<number>}
   */
  _chartXMin: computed('_expectedStatsNumber', '_timePeriod', function () {
    const {
      _timePeriod,
      _expectedStatsNumber,
    } = this.getProperties(
      '_timePeriod',
      '_expectedStatsNumber'
    );
    return -_timePeriod * _expectedStatsNumber;
  }),

  /**
   * Array of x coordinates for chart points
   * @type {Ember.ComputedProperty<Array<number>>}
   */
  _chartPointsXValues: computed(
    '_timePeriod',
    '_statEndTime',
    '_expectedStatsNumber',
    function () {
      let {
        _timePeriod,
        _statEndTime,
        _expectedStatsNumber,
      } = this.getProperties(
        '_timePeriod',
        '_statEndTime',
        '_expectedStatsNumber'
      );
      let x = 0;
      const xValues = [];
      xValues.push(x);
      _statEndTime += 1;
      x = _statEndTime % _timePeriod === 0 ? -_timePeriod : -(_statEndTime % _timePeriod);
      for (let i = 1;;i++) {
        xValues.push(x);
        const newX = Math.max(
          x - _timePeriod,
          -_timePeriod * _expectedStatsNumber
        );
        if (newX === x) {
          break;
        } else {
          x = newX;
        }
      }
      return xValues;
    }
  ),

  /**
   * Number of points on chart.
   * @type {Ember.ComputedProperty<number>}
   */
  _pointsNumber: computed.reads('_chartPointsXValues.length'),

  /**
   * Maximum stats sum in all time slots
   * @type {Ember.ComputedProperty<number>}
   */
  _chartYMax: computed('_maxStatsInSum', '_maxStatsOutSum', function () {
    const {
      _maxStatsInSum,
      _maxStatsOutSum,
    } = this.getProperties('_maxStatsInSum', '_maxStatsOutSum');
    return Math.max(_maxStatsInSum, _maxStatsOutSum, 8);
  }),

  /**
   * Chart ticks for Y axis
   * @type {Ember.ComputedProperty<number>}
   */
  _chartYTicks: computed('_chartYMax', function () {
    const numberOfTicksPerSide = 3;
    const _chartYMax = this.get('_chartYMax');
    const delta = _chartYMax / numberOfTicksPerSide;
    const ticks = _.range(numberOfTicksPerSide).map(i => delta * (i + 1));
    return ticks.concat([0], ticks.map(n => n * -1).reverse());
  }),

  /**
   * Chartist settings
   * @type {Object}
   */
  _chartOptions: computed(
    '_chartXTicks',
    '_chartXMin',
    '_chartYTicks',
    '_chartYMax',
    function () {
      const {
        _chartXTicks,
        _chartXMin,
        _chartYTicks,
        _chartYMax,
      } = this.getProperties(
        '_chartXTicks',
        '_chartXMin',
        '_chartYTicks',
        '_chartYMax'
      );
      return {
        axisX: {
          low: _chartXMin,
          high: 0,
          type: Chartist.FixedScaleAxis,
          ticks: _chartXTicks,
          labelInterpolationFnc: value => this._formatXAxisLabel(value),
          showGrid: false,
        },
        axisY: {
          low: -_chartYMax,
          high: _chartYMax,
          type: Chartist.FixedScaleAxis,
          labelInterpolationFnc:
            value => bytesToString(Math.abs(value), { format: 'bit' }) + 'ps',
          ticks: _chartYTicks,
          position: 'end',
        },
        showArea: true,
        fullWidth: true,
        chartPadding: {
          top: 30,
          bottom: 15,
          left: 30,
          right: 40,
        },
        plugins: [
          stackedLineMask(),
          customCss({
            filterBySeriesIndex: true,
          }),
          centerXLabels(),
          axisLabels({
            xLabelYOffset: -10,
            xLabelXOffset: -20,
            yAlignment: 'right',
            yLabelXOffset: -10,
            yLabelYOffset: 0,
          }),
          eventListener({
            eventHandler: (eventData) => this._chartEventHandler(eventData),
          }),
        ],
      };
    }
  ),

  /**
   * Data for chartist (async -> _statsValues)
   * @type {Ember.ComputedProperty<Object|undefined>}
   */
  _chartData: computed(
    '_statsValues',
    '_sortedProvidersIds',
    'providersColors',
    '_pointsNumber',
    function () {
      const {
        _statsValues,
        _chartValues,
        _sortedProvidersIds,
        providersColors,
        _statsIn,
        _pointsNumber,
        i18n,
        _statEndTime,
      } = this.getProperties(
        '_statsValues',
        '_chartValues',
        '_sortedProvidersIds',
        'providersColors',
        '_statsIn',
        '_pointsNumber',
        'i18n',
        '_statEndTime'
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
        let valuesSumArray = _.range(_pointsNumber).map(() => ({ x: 0, y: 0 }));
        for (let i = _statsValues.length - 1; i >= 0; i--) {
          if (Object.keys(_statsIn).length === i + 1) {
            valuesSumArray = _.range(_pointsNumber).map(() => ({ x: 0, y: 0 }));
          }
          /* jshint loopfunc: true */
          const providerValues = _statsValues[i];
          providerValues.forEach((value, valueIndex) => {
            valuesSumArray[valueIndex].y += value.y;
            valuesSumArray[valueIndex].x = value.x;
          });
          _chartValues[i]
            .push(..._.cloneDeep(
              valuesSumArray
            ));
        }
        // setting colors
        const customCss = _sortedProvidersIds.map((providerId) => {
          const color = providersColors[providerId] || providersColors['unknown'];
          return _.times(_pointsNumber, _.constant({
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
          axisLabels: {
            xLabel: i18n.t(
              I18N_PREFIX + 'timeLastUpdate',
              { lastUpdate: this._formatStatTime(_statEndTime + 30, 'HH:mm:ss') }
            ),
            yLabel: i18n.t(I18N_PREFIX + 'throughput'),
          }
        }; 
      }
    }
  ),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  _tooltipHeader: computed(
    '_statEndTime',
    'chartTooltipHoveredColumn',
    '_chartPointsXValues',
    function () {
      let {
        _statEndTime,
        chartTooltipHoveredColumn,
        _chartPointsXValues,
      } = this.getProperties(
        '_statEndTime',
        'chartTooltipHoveredColumn',
        '_chartPointsXValues'
      );
      _statEndTime += 1;
      const xDiff = _chartPointsXValues.slice(0).reverse();
      const index = chartTooltipHoveredColumn;
      const startTime = this._formatStatTime(_statEndTime + xDiff[index]);
      const endTime = this._formatStatTime(_statEndTime + xDiff[index - 1]);
      if (index === 0 || startTime === endTime) {
        return startTime;
      } else {
        return endTime + ' - ' + startTime;
      }
    }
  ),

  /**
   * @type {Ember.ComputedProperty<Array<object>>}
   */
  _tooltipInProviders: computed(
    '_sortedInProvidersIds',
    'providersColors',
    'providers',
    '_statsIn',
    'chartTooltipHoveredColumn',
    function () {
      const {
        _sortedInProvidersIds,
        _statsIn,
      } = this.getProperties(
        '_sortedInProvidersIds',
        '_statsIn'
      );
      return this._generateTooltipItems(_statsIn, _sortedInProvidersIds);
    }
  ),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  _tooltipInSum: computedPipe('_tooltipInProviders', '_generateTooltipItemsSum'),

  /**
   * @type {Ember.ComputedProperty<Array<object>>}
   */
  _tooltipOutProviders: computed(
    '_sortedOutProvidersIds',
    'providersColors',
    'providers',
    '_statsOut',
    'chartTooltipHoveredColumn',
    function () {
      const {
        _sortedOutProvidersIds,
        _statsOut,
      } = this.getProperties(
        '_sortedOutProvidersIds',
        '_statsOut'
      );
      return this._generateTooltipItems(_statsOut, _sortedOutProvidersIds);
    }
  ),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  _tooltipOutSum: computedPipe('_tooltipOutProviders', '_generateTooltipItemsSum'),
  
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
    this._createTimeStatsUpdater();
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
    const gettingStats = this.get('_timeStatForUnit');
 
    console.log('throughput-distribution: creating updater');
    gettingStats.then(timeStat => {
      this.setProperties({
        _statsError: null,
        updater: TransferTimeStatUpdater.create({
          isEnabled: this.get('_updaterEnabled'),
          timeStat,
        }),
      });
    });
    gettingStats.catch(error => {
      this.set('_statsError', error);
    });
  },
  
  /**
   * Calculates throughput value for given bytes number and time step index
   * @param {Array<number>} statValue transfered bytes/s for chart value
   * @returns {number} average throughput in bytes per second
   */
  _scaleStatValue(statValues) {
    const {
      _chartPointsXValues,
    } = this.getProperties(
      '_chartPointsXValues'
    );
    return _chartPointsXValues.map((x, i) => ({
      x,
      y: statValues[i],
    })).reverse();
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

  /**
   * Formats timestamp according to format or _timeFormat
   * @param {number} time timestamp
   * @param {string} format
   */
  _formatStatTime(time, format=undefined) {
    return moment.unix(time).format(format || this.get('_timeFormat'));
  },

  /**
   * Formats time duration to fit x axis labels format
   * @param {number} time time duration in seconds
   */
  _formatXAxisLabel(time) {
    const duration = moment.duration(time, 'seconds');
    const timeUnit = this.get('timeUnit');
    if (timeUnit === 'minute') {
      // minute stats are delayed by 30 seconds
      duration.subtract(30, 'seconds');
    }
    return duration.as(subunit[timeUnit] + 's') + subunitSuffix[timeUnit];
  },

  /**
   * Calculates max sum of values in whole stats object
   * @param {object} stats 
   */
  _calculateStatsMaxSum(stats) {
    const arrays = _.values(stats);
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
    return maxSum;
  },

  /**
   * Removes zero series from stats
   * @param {object} stats 
   */
  _removeZeroStats(stats) {
    if (!stats) {
      return {};
    }
    const reducedStats = {};
    Object.keys(stats)
      .filter((providerId) => stats[providerId].some((s) => !!s))
      .forEach((providerId) => reducedStats[providerId] = stats[providerId]);
    return reducedStats;
  },

  /**
   * Generates list of objects, that is used to display list of providers
   * @param {object} stats stats used to generate items
   * @param {array} providersIds list of sorted provider ids
   */
  _generateTooltipItems(stats, providersIds) {
    const {
      providersColors,
      providers,
      chartTooltipHoveredColumn,
      _pointsNumber,
    } = this.getProperties(
      'providersColors',
      'providers',
      'chartTooltipHoveredColumn',
      '_pointsNumber'
    );
    const result = [];
    providersIds.forEach(providerId => {
      const providerStats = stats[providerId];
      const index = _pointsNumber - chartTooltipHoveredColumn - 1;
      if (index < 0 || !providerStats[index]) {
        return;
      }
      const provider =
        _.find(providers, (provider) => provider.get('id') === providerId) || {};
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
  },

  /**
   * Calculates sum of legend elements displayed in tooltip
   * @param {array<object>} items array of object used to create tooltip items
   */
  _generateTooltipItemsSum(items) {
    const bytes = _.sum(items.map(p => p.valueNumber));
    return bytesToString(bytes, { format: 'bit' }) + 'ps';
  },

  /**
   * It positions descriptions for halves of the chart
   */
  _positionHalvesDescriptions() {
    const verticalGrid = this.$('.ct-grid.ct-vertical');
    const highestLineY = parseFloat(verticalGrid.last().attr('y1'));
    const lowestLineY = parseFloat(verticalGrid.first().attr('y1'));
    const midLineY = (highestLineY + lowestLineY) / 2;
    const halfDescriptionWidth = lowestLineY - midLineY;
    this.$('.output-half').css({
      top: ((lowestLineY + midLineY) / 2) + 'px',
      width: halfDescriptionWidth + 'px',
    }).show();
    this.$('.input-half').css({
      top: ((highestLineY + midLineY) / 2) + 'px',
      width: halfDescriptionWidth + 'px',
    }).show();
  },

  /**
   * Makes X0 axis bold
   */
  _boldX0Axis() {
    const verticalGrid = this.$('.ct-grid.ct-vertical');
    $(verticalGrid.get(Math.floor(verticalGrid.length / 2))).addClass('x-axis-line');
  },

  /**
   * Attaches all needed handlers to the chart
   * @param {object} param event data
   */
  _chartEventHandler(eventData) {
    const {
      eventName,
    } = eventData;
    this.addChartValuesLine(eventData);
    this.addChartTooltip(eventData);
    safeExec(this, () => {
      if (eventName === 'created') {
        this._positionHalvesDescriptions();
        this._boldX0Axis();
      }
    });
  },
});
