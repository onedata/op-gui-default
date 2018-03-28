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

const {
  Component,
  computed,
  get,
  observer,
  inject: {
    service,
  },
  run,
  String :{
    htmlSafe
  },
} = Ember;

const I18N_PREFIX = 'components.transfers.throughputDistributionChart.';

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

export default Component.extend({
  classNames: ['transfers-throughput-distribution'],
  i18n: service(),

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
   * One of `minute`, `hour`, `day`, `month`.
   * @type {string}
   */
  timeUnit: 'minute',

  /**
   * Colors used to color each providers' series
   * @virtual
   * @type {Ember.ComputedProperty<Object>}
   */
  providersColors: undefined,
  
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
   * Proxy object that resolves with stats for specified time unit.
   * @type {Ember.ComputedProperty<PromiseObject<SpaceTransferTimeStat>>}
   */
  _timeStatForUnit: computed('space', 'timeUnit', function () {
    const {
      space,
      timeUnit,
    } = this.getProperties('space', 'timeUnit');
    const unitProp = `transfer${_.capitalize(timeUnit)}Stat`;
    return PromiseObject.create({ promise: get(space, unitProp) });
  }),

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
      const {
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
          const color = providersColors[providerId];
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
   * @type {boolean}
   */
  _chartCreated: false,

  /**
   * @type {Array<number>}
   */
  _pointsColumnXPosition: [],

  /**
   * @type {Array<number>}
   */
  _pointsColumnYPosition: [],

  /**
   * @type {boolean}
   */
  _tooltipVisible: false,

  /**
   * @type {number}
   */
  _hoveredPointsColumnIndex: -1,

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  _tooltipHeader: computed(
    '_statEndTime',
    '_hoveredPointsColumnIndex',
    '_chartPointsXValues',
    function () {
      const {
        _statEndTime,
        _hoveredPointsColumnIndex,
        _chartPointsXValues,
      } = this.getProperties(
        '_statEndTime',
        '_hoveredPointsColumnIndex',
        '_chartPointsXValues'
      );
      const xDiff = _chartPointsXValues.slice(0).reverse();
      const index = _hoveredPointsColumnIndex;
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
    '_hoveredPointsColumnIndex',
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
    '_hoveredPointsColumnIndex',
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
      _hoveredPointsColumnIndex,
      _pointsNumber,
    } = this.getProperties(
      'providersColors',
      'providers',
      '_hoveredPointsColumnIndex',
      '_pointsNumber'
    );
    const result = [];
    providersIds.forEach(providerId => {
      const providerStats = stats[providerId];
      const index = _pointsNumber - _hoveredPointsColumnIndex - 1;
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
        boxStyle: htmlSafe('background-color: ' + providersColors[providerId]),
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
   * Resets all remembered data related to the chart svg
   */
  _resetRememberedChartState() {
    this.setProperties({
      _chartCreated: false,
      _pointsColumnXPosition: [],
      _pointsColumnYPosition: [],
    });
  },

  /**
   * Persists coordinates of drawed point
   * @param {object} data chart event data
   */
  _rememberChartPointCoordinates(data) {
    const {
      _pointsColumnXPosition,
      _pointsColumnYPosition,
     } = this.getProperties('_pointsColumnXPosition', '_pointsColumnYPosition');
    const pointNode = data.element.getNode();
    const pointXCenter = (pointNode.x1.baseVal.value + pointNode.x2.baseVal.value) / 2;
    const pointYCenter = (pointNode.y1.baseVal.value + pointNode.y2.baseVal.value) / 2;
    if (!_pointsColumnXPosition[data.index]) {
      _pointsColumnXPosition[data.index] = pointXCenter;
    }
    if (typeof _pointsColumnYPosition[data.index] !== 'number' ||
      _pointsColumnYPosition[data.index] > pointYCenter) {
      _pointsColumnYPosition[data.index] = pointYCenter;
    }
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
   * Adds values line to the chart svg
   * @param {object} chart chart object
   */
  _addValuesLineToChart(chart) {
    const verticalGrid = this.$('.ct-grid.ct-vertical');
    const highestLineY = parseFloat(verticalGrid.last().attr('y1'));
    const lowestLineY = parseFloat(verticalGrid.first().attr('y1'));
    const valuesLineGroup = chart.svg.elem('g', { class: 'ct-values-line-group' });
    valuesLineGroup.elem('line', {
      y1: highestLineY,
      y2: lowestLineY,
      x1: 0,
      x2: 0,
      class: 'ct-values-line',
    });
  },

  /**
   * Makes X0 axis bold
   */
  _boldX0Axis() {
    const verticalGrid = this.$('.ct-grid.ct-vertical');
    $(verticalGrid.get(Math.floor(verticalGrid.length / 2))).addClass('x-axis-line');
  },

  /**
   * Identifies hovered values column using given event
   * @param {MouseEvent} event Mouse move event
   * @param {object} chart chart object
   */
  _identifyHoveredValuesColumn(event, chart) {
    const _pointsColumnXPosition = this.get('_pointsColumnXPosition');
    const chartContainer = $(chart.container);
    const mouseX = event.pageX - chartContainer.offset().left;
    if (mouseX < _pointsColumnXPosition[0] ||
      mouseX > _pointsColumnXPosition[_pointsColumnXPosition.length - 1] ||
      _pointsColumnXPosition.length === 0) {
      this.set('_hoveredPointsColumnIndex', -1);
    } else {
      let targetIndex = 0;
      let targetIndexDistance = Math.abs(mouseX - _pointsColumnXPosition[0]);
      for (let i = 1; i < _pointsColumnXPosition.length; i++) {
        const distance = Math.abs(mouseX - _pointsColumnXPosition[i]);
        if (distance < targetIndexDistance) {
          targetIndex = i;
          targetIndexDistance = distance;
        }
      }
      this.set('_hoveredPointsColumnIndex', targetIndex);
    }
  },

  /**
   * Attaches listeners, that are responsible for monitoring actual hovered
   * values column
   * @param {object} chart chart object
   */
  _attachValuesColumnHoverListeners(chart) {
    $(chart.container).mousemove((event) => run(() => {
      safeExec(this, () => {
        this._identifyHoveredValuesColumn(event, chart);
        this._updateChartHoverState();
      });
    }))
    .mouseleave(() => run(() => {
      safeExec(this, () => {
        this.set('_hoveredPointsColumnIndex', -1);
        this._updateChartHoverState();
      });
    }));
  },

  /**
   * Attaches all needed handlers to the chart
   * @param {object} param event data
   */
  _chartEventHandler({ eventName, data, chart }) {
    safeExec(this, () => {
      if ((eventName === 'draw' && this.get('_chartCreated')) ||
        data.type === 'initial') {
        this._resetRememberedChartState();
      }
      if (eventName === 'draw' && data.type === 'point') {
          this._rememberChartPointCoordinates(data);
      }
      if (eventName === 'created') {
        this._positionHalvesDescriptions();
        this._addValuesLineToChart(chart);
        this._boldX0Axis();
        this._attachValuesColumnHoverListeners(chart);
        this._updateChartHoverState();
        this.set('_chartCreated', true);
      }
    });
  },

  /**
   * Updates chart appearance on mouse event
   */
  _updateChartHoverState() {
    this._showValuesLineIfNeeded();
    this._showTooltipIfNeeded();
  },

  /**
   * Shows/hides value line
   */
  _showValuesLineIfNeeded() {
    const line = this.$('.ct-values-line');
    if (!line.length) {
      return;
    }
    const _hoveredPointsColumnIndex = this.get('_hoveredPointsColumnIndex');
    this.$('.ct-point.ct-point-active').removeClass('ct-point-active');
    if (_hoveredPointsColumnIndex !== -1) {
      const x = this.get('_pointsColumnXPosition')[_hoveredPointsColumnIndex];
      line.addClass('ct-values-line-active');
      line.attr('x1', x);
      line.attr('x2', x);
      this.$('.ct-series').toArray().forEach(group => {
        const point = $(group).find('.ct-point').get(_hoveredPointsColumnIndex);
        $(point).addClass('ct-point-active');
      });
    } else {
      line.removeClass('ct-values-line-active');
    }
  },

  /**
   * Shows/hides tooltip
   */
  _showTooltipIfNeeded() {
    const tooltip = this.$('.ct-tooltip');
    if (!tooltip.length) {
      return;
    }
    const _hoveredPointsColumnIndex = this.get('_hoveredPointsColumnIndex');
    if (_hoveredPointsColumnIndex !== -1) {
      const {
        _pointsColumnXPosition,
        _pointsColumnYPosition,
      } = this.getProperties('_pointsColumnXPosition', '_pointsColumnYPosition');
      const chartContainer = this.$('.ct-chart');
      const chartXCenter = chartContainer.width() / 2;
      tooltip.css({
        top: _pointsColumnYPosition[_hoveredPointsColumnIndex] + 'px',
        left: _pointsColumnXPosition[_hoveredPointsColumnIndex] + 'px',
      });
      if (_pointsColumnXPosition[_hoveredPointsColumnIndex] < chartXCenter) {
        tooltip.addClass('right').removeClass('left');
      } else {
        tooltip.addClass('left').removeClass('right');
      }
      tooltip.show();
    } else {
      tooltip.hide();
    }
  }
});
