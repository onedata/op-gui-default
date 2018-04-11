/**
 * Adds tooltip to chart.
 * 
 * To enable, addChartTooltip must be called in chartist eventListener plugin
 * and chartTooltipSelector must be set.
 * 
 * @module mixins/components/chartist-tooltip
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import identifyHoveredValuesColumn from 'op-worker-gui/utils/chartist/identify-hovered-values-column';

const {
  run,
  computed: {
    reads,
  },
} = Ember;

export default Ember.Mixin.create({
  /**
   * @virtual
   * @type {string}
   */
  chartTooltipSelector: undefined,

  /**
   * Possible values: top, bottom
   * @virtual
   * @type {string}
   */
  chartTooltipVerticalAlign: 'bottom',

  /**
   * Hovered column index (counting from the left side of the chart)
   * @type {Ember.ComputedProperty<boolean>}
   */
  chartTooltipHoveredColumn: reads('_ctHoveredPointsColumnIndex'),

  /**
   * @type {Ember.ComputedProperty<number>}
   */
  chartTooltipColumnsNumber: reads('_ctPointsColumnXPosition.length'),

  /**
   * @type {boolean}
   */
  _ctChartCreated: false,

  /**
   * @type {Array<number>}
   */
  _ctPointsColumnXPosition: Object.freeze([]),

  /**
   * @type {Array<number>}
   */
  _ctPointsColumnYPosition: Object.freeze([]),

  /**
   * @type {number}
   */
  _ctHoveredPointsColumnIndex: -1,

  /**
   * Adds needed listeners to draw values line
   * @param {object} eventData chartist event data
   */
  addChartTooltip(eventData) {
    safeExec(this, '_ctEventHandler', eventData);
  },

  /**
   * Event handler
   * @param {object} eventData chartist event data
   */
  _ctEventHandler(eventData) {
    const {
      eventName,
      data,
      chart,
    } = eventData;
    if ((eventName === 'draw' && this.get('_ctChartCreated')) ||
      data.type === 'initial') {
        this.setProperties({
          _ctChartCreated: false,
          _ctPointsColumnXPosition: [],
          _ctPointsColumnYPosition: [],
        });
    }
    if (eventName === 'draw' && data.type === 'point') {
      this._ctRememberChartPointCoordinates(data);
    }
    if (eventName === 'created') {
      this._ctAttachValuesColumnHoverListeners(chart);
      this._ctShowTooltipIfNeeded();
      this.set('_ctChartCreated', true);
    }
  },

  /**
   * Persists coordinates of drawed point
   * @param {object} data chart event data
   */
  _ctRememberChartPointCoordinates(data) {
    const {
      _ctPointsColumnXPosition,
      _ctPointsColumnYPosition,
      chartTooltipVerticalAlign,
     } = this.getProperties(
       '_ctPointsColumnXPosition',
       '_ctPointsColumnYPosition',
       'chartTooltipVerticalAlign');
    const pointNode = data.element.getNode();
    const pointXCenter = (pointNode.x1.baseVal.value + pointNode.x2.baseVal.value) / 2;
    const pointYCenter = (pointNode.y1.baseVal.value + pointNode.y2.baseVal.value) / 2;
    if (!_ctPointsColumnXPosition[data.index]) {
      _ctPointsColumnXPosition[data.index] = pointXCenter;
    }
    if (typeof _ctPointsColumnYPosition[data.index] !== 'number' ||
      (chartTooltipVerticalAlign === 'bottom' && _ctPointsColumnYPosition[data.index] > pointYCenter ||
      chartTooltipVerticalAlign === 'top' && _ctPointsColumnYPosition[data.index] < pointYCenter)) {
      _ctPointsColumnYPosition[data.index] = pointYCenter;
    }
  },

  /**
   * Attaches listeners, that are responsible for monitoring actual hovered
   * values column
   * @param {object} chart chart object
   */
  _ctAttachValuesColumnHoverListeners(chart) {
    $(chart.container).mousemove((event) => run(() => {
      safeExec(this, () => {
        this.set(
          '_ctHoveredPointsColumnIndex',
          identifyHoveredValuesColumn(event, chart, this.get('_ctPointsColumnXPosition'))
        );
        this._ctShowTooltipIfNeeded();
      });
    }))
    .mouseleave(() => run(() => {
      safeExec(this, () => {
        this.set('_ctHoveredPointsColumnIndex', -1);
        this._ctShowTooltipIfNeeded();
      });
    }));
  },

  /**
   * Shows/hides tooltip
   */
  _ctShowTooltipIfNeeded() {
    const {
      _ctHoveredPointsColumnIndex,
      chartTooltipSelector,
      chartTooltipVerticalAlign,
    } = this.getProperties(
      '_ctHoveredPointsColumnIndex',
      'chartTooltipSelector',
      'chartTooltipVerticalAlign'
    );
    const tooltip = this.$(chartTooltipSelector);
    if (!tooltip.length) {
      return;
    }
    if (_ctHoveredPointsColumnIndex !== -1) {
      const {
        _ctPointsColumnXPosition,
        _ctPointsColumnYPosition,
      } = this.getProperties('_ctPointsColumnXPosition', '_ctPointsColumnYPosition');
      const chartContainer = this.$('.ct-chart');
      const chartXCenter = chartContainer.width() / 2;
      tooltip.css({
        top: _ctPointsColumnYPosition[_ctHoveredPointsColumnIndex] + 'px',
        left: _ctPointsColumnXPosition[_ctHoveredPointsColumnIndex] + 'px',
      });
      if (_ctPointsColumnXPosition[_ctHoveredPointsColumnIndex] < chartXCenter) {
        tooltip.addClass('right').removeClass('left');
      } else {
        tooltip.addClass('left').removeClass('right');
      }
      if (chartTooltipVerticalAlign === 'top') {
        tooltip.addClass('top').removeClass('bottom');
      } else {
        tooltip.addClass('bottom').removeClass('top');
      }
      tooltip.show();
    } else {
      tooltip.hide();
    }
  }
});
