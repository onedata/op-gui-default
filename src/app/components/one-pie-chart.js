/**
 * A component that renders a pie chart using series definition from `data` 
 * property. Hovered series can be set from the outside by specifying 
 * activeSeriesId property.
 *
 * @module components/one-pie-chart
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * @typedef {Ember.Object} PieChartSeries A series displayed in a chart
 * @property {string} id An id for the series (must be unique across all 
 * chart series).
 * @property {string} label A label for the series.
 * @property {number} value A value to display.
 * @property {string} color A color for series.
 */

/* global Chartist */

import Ember from 'ember';
import _ from 'lodash';
import centeredText from 'op-worker-gui/utils/chartist/centered-text';
import pieLabels from 'op-worker-gui/utils/chartist/pie-labels';
import tooltip from 'op-worker-gui/utils/chartist/tooltip';
import customCss from 'op-worker-gui/utils/chartist/custom-css';
import legendColors from 'op-worker-gui/utils/chartist/legend-colors';

const {
  computed,
  A,
  run: {
    debounce,
  },
} = Ember;

const INACTIVE_SERIES_OPACITY = 0.3;
const SERIES_HOVER_TRANSITION_TIME = 0.3;

export default Ember.Component.extend({
  classNames: ['one-pie-chart'],

  /**
   * Data for a chart.
   * @type {Ember.Array.PieChartSeries}
   */
  data: null,

  /**
   * If true, data is valid and can be used to generate a chart.
   * @type {computed.boolean}
   */
  isDataValid: computed(() => true),

  /**
   * If true, series will be sorted by value.
   * @type {boolean}
   */
  sort: false,

  /**
   * If true, series sort will be descending.
   * @type {boolean}
   */
  sortDescending: true,

  /**
   * Css opacity value of inactive (not hovered) slice.
   * @type {number}
   */
  inactiveOpacity: INACTIVE_SERIES_OPACITY,

  /**
   * Css transition time value for hovered slice animation (in seconds).
   * @type {number}
   */
  hoverTransitionTime: SERIES_HOVER_TRANSITION_TIME,

  /**
   * Id of hovered (active) series.
   * @type {string}
   */
  activeSeriesId: null,

  /**
   * If true, component is displayed using mobile configuration
   * @type {boolean}
   */
  _mobileMode: false,

  /**
   * Timeout id for styles recompute (see _chartCss property)
   * @type {number}
   */
  _stylesRecomputeTimeoutId: -1,

  /**
   * Window resize handler.
   * @type {Function}
   */
  _windowResizeHandler: computed(function () {
    return () => {
      debounce(this, this._windowResized, 100);
    };
  }),

  /**
   * Sorted data.
   * @type {computed.Ember.Array.PieChartSeries}
   */
  _sortedData: computed('data.[]', 'sort', 'sortDescending', 'isDataValid',
    function () {
      let {
        data,
        sort,
        isDataValid,
      } = this.getProperties('data', 'sort', 'isDataValid');
      if (isDataValid) {
        return sort ? this.sortData(data) : data;
      } else {
        return A();
      }
    }),

  /**
   * All series values sum.
   * @type {computed.number}
   */
  _valuesSum: computed('data.@each.value', function () {
    return this.get('data').reduce((sum, series) => sum + series.get('value'), 0);
  }),

  /**
   * Chartist options.
   * @type {computed.Object}
   */
  _chartOptions: computed('_sortedData', '_valuesSum', '_mobileMode', function () {
    return this.generateChartOptions();
  }),

  /**
   * Chartist chart series
   * @type {computed.Array.Object}
   */
  _chartDataSeries: computed('_sortedData.@each.value', function () {
    return this.generateChartDataSeries();
  }),

  /**
   * Chartist chart pie labels
   * @type {computed.Array.string}
   */
  _chartPieLabels: computed('_sortedData.@each.label', function () {
    return this.generateChartPieLabels();
  }),

  /**
   * Chartist chart css
   * @type {computed.Object}
   */
  _chartCss: computed('_sortedData.[]', 'activeSeriesId', 'inactiveOpacity',
    'hoverTransitionTime', {
      get() {
        let {
          hoverTransitionTime,
          _stylesRecomputeTimeoutId,
        } = this.getProperties(
          'hoverTransitionTime',
          '_stylesRecomputeTimeoutId'
        );
        clearTimeout(_stylesRecomputeTimeoutId);
        this.set('_stylesRecomputeTimeoutId', setTimeout(
          () => {
            if (!this.isDestroyed && !this.isDestroying) {
              this.set('_chartCss', this.generateChartStyles());
            }
          },
          hoverTransitionTime * 1000
        ));
        return this.generateChartStyles();
      },
      set(key, value) {
        return value;
      }
    }
  ),

  /**
   * Chartist data
   * @type {computed.Object}
   */
  _chartData: computed('_chartDataLabels', '_chartDataSeries', '_chartCss',
    '_chartOptions',
    function () {
      return this.generateChartData();
    }
  ),

  didInsertElement() {
    this._super(...arguments);
    $(window).on('resize', this.get('_windowResizeHandler'));
    this._windowResized();
    this.$('.ct-chart').mousemove((event) => {
      let parentGroup = $(event.target).parents('.ct-series');
      if (parentGroup.length) {
        // extract series id from group class name `slice-id-[series.id]`
        let sliceClass = _.find(
          parentGroup.attr('class').split(' '),
          (c) => c.startsWith('slice-id-')
        );
        let seriesId = sliceClass.substr('slice-id-'.length);
        this.set('activeSeriesId', seriesId);
      } else {
        // if label is hovered, ignore series hover change
        parentGroup = $(event.target).parents('.ct-pie-label');
        if (parentGroup.length === 0) {
          this.set('activeSeriesId', null);
        }
      }
    });
  },

  willDestroyElement() {
    try {
      $(window).off('resize', this.get('_windowResizeHandler'));
    } finally {
      this._super(...arguments);
    }
  },

  /**
   * Sorts data.
   * @param {Ember.Array.PieChartSeries} data The chart data.
   * @returns {Ember.Array.PieChartSeries} A sorted data.
   */
  sortData(data) {
    let sortDescending = this.get('sortDescending');
    let sortedData = A(data.sortBy('value'));
    return sortDescending ? sortedData.reverseObjects() : sortedData;
  },

  /**
   * Returns value as a string.
   * @param {number} value A value.
   * @return {string} A value string.
   */
  formatValue(sum) {
    return String(sum);
  },

  /**
   * Creates chartist options object.
   * @returns {Object} Chartist options.
   */
  generateChartOptions() {
    let {
      _mobileMode,
      _valuesSum,
      _sortedData,
    } = this.getProperties('_mobileMode', '_valuesSum', '_sortedData');
    let optionsBase = {
      donut: true,
      donutWidth: '45%',
      showLabel: false,
      chartPadding: 20,
      plugins: [
        centeredText({
          text: this.formatValue(_valuesSum),
        }),
        tooltip({
          chartType: 'pie',
        }),
        pieLabels({
          hideLabelThresholdPercent: 0,
        }),
        customCss(),
        Chartist.plugins.legend({
          legendNames: _.map(_sortedData, 'label'),
          className: 'not-clickable',
          clickable: false,
        }),
        legendColors({
          colors: _.map(_sortedData, 'color'),
        }),
      ]
    };
    if (_mobileMode) {
      optionsBase.disabledPlugins = ['pieLabels'];
    } else {
      optionsBase.disabledPlugins = ['tooltip'];
    }
    return optionsBase;
  },

  /**
   * Creates chartist data object.
   * @returns {Object} Chartist data.
   */
  generateChartData() {
    let {
      _chartPieLabels,
      _chartDataSeries,
      _chartCss,
      _sortedData,
    } = this.getProperties(
      '_chartPieLabels',
      '_chartDataSeries',
      '_chartCss',
      '_sortedData'
    );
    return {
      labels: _.map(_sortedData, 'label'),
      pieLabels: _chartPieLabels,
      series: _chartDataSeries,
      customCss: _chartCss,
    };
  },

  /**
   * Creates chartist data series for data object.
   * @returns {Array.Object} Chartist data series.
   */
  generateChartDataSeries() {
    let _sortedData = this.get('_sortedData');
    return _sortedData.map((series) => {
      return {
        data: series.get('value'),
        className: 'slice-id-' + series.get('id'),
        tooltipElements: [{
          name: 'Value',
          value: this.formatValue(series.get('value')),
        }],
      };
    });
  },

  /**
   * Creates chartist labels for data object.
   * @returns {Array.Object} Chartist labels.
   */
  generateChartPieLabels() {
    let _sortedData = this.get('_sortedData');
    return _sortedData.map((series) => {
      let className = 'label-id-' + series.get('id');
      if (this.getSeriesPercentSize(series) <= 0.15) {
        className += ' label-hidden';
      }
      // This object is not compatible with standard chartist label renderer.
      // It is specific for pie-labels plugin.
      return {
        topText: series.get('label'),
        bottomText: this.formatValue(series.get('value')),
        className,
      };
    });
  },

  /**
   * Creates a styles object for the chart.
   * @returns {Array.Object} Styles specification.
   */
  generateChartStyles() {
    let {
      _sortedData,
      activeSeriesId,
      inactiveOpacity,
      hoverTransitionTime,
    } = this.getProperties(
      '_sortedData',
      'activeSeriesId',
      'inactiveOpacity',
      'hoverTransitionTime'
    );
    return _sortedData.map((series) => {
      // isActive = is nothing or this series hovered
      let isActive = activeSeriesId === series.get('id');
      let isLabelVisible = isActive || (!activeSeriesId &&
        this.getSeriesPercentSize(series) > 0.15);
      // actual values of label opacity and slice stroke-opacity are
      // remembered to save animation state through chart rerender
      return {
        'slice': {
          'stroke': series.get('color') || null,
          'stroke-opacity': this._getSliceOpacity(series),
          'transitionProperties': {
            'transition': `stroke-opacity ${hoverTransitionTime}s`,
            'stroke-opacity': !activeSeriesId || isActive ?
              '1' : String(inactiveOpacity),
          }
        },
        'pie-label': {
          'opacity': this._getLabelOpacity(series),
          'pointer-events': isLabelVisible ? 'initial' : 'none',
          'transitionProperties': {
            'transition': `opacity ${hoverTransitionTime}s`,
            'opacity': isLabelVisible ? '1' : '0',
          }
        }
      };
    });
  },

  /**
   * Returns series share in overall values sum.
   * @param {PieChartSeries} series A series.
   * @returns {number} A value 0 < x <= 1
   */
  getSeriesPercentSize(series) {
    return series.get('value') / this.get('_valuesSum');
  },

  /**
   * Returns actual series slice opacity.
   * @param {PieChartSeries} series A series.
   * @returns {string} stroke-opacity value
   */
  _getSliceOpacity(series) {
    return this.$(`.slice-id-${series.get('id')} path`).css('stroke-opacity');
  },

  /**
   * Returns actual series label opacity.
   * @param {PieChartSeries} series A series.
   * @returns {string} opacity value
   */
  _getLabelOpacity(series) {
    return this.$('.label-id-' + series.get('id')).css('opacity');
  },

  /**
   * Checks if the browser window has mobile width or not
   */
  _windowResized() {
    this.set('_mobileMode', window.innerWidth < 768);
  }
});
