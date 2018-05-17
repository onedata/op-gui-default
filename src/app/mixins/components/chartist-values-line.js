/**
 * Adds vertical values line to chart.
 * 
 * To enable, addChartValuesLine must be called in chartist eventListener plugin.
 * 
 * @module mixins/components/chartist-values-line
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import identifyHoveredValuesColumn from 'op-worker-gui/utils/chartist/identify-hovered-values-column';

const {
  run,
} = Ember;

export default Ember.Mixin.create({
  /**
   * @type {boolean}
   */
  _cvlChartCreated: false,

  /**
   * @type {Array<number>}
   */
  _cvlPointsColumnXPosition: Object.freeze([]),

  /**
   * @type {number}
   */
  _cvlHoveredPointsColumnIndex: -1,

  /**
   * Adds needed listeners to draw values line
   * @param {object} eventData chartist event data
   */
  addChartValuesLine(eventData) {
    safeExec(this, '_cvlEventHandler', eventData);
  },

  /**
   * Event handler
   * @param {object} eventData chartist event data
   */
  _cvlEventHandler(eventData) {
    const {
      eventName,
      data,
      chart,
    } = eventData;
    if ((eventName === 'draw' && this.get('_cvlChartCreated')) ||
      data.type === 'initial') {
        this.setProperties({
          _cvlChartCreated: false,
          _cvlPointsColumnXPosition: [],
        });
    }
    if (eventName === 'draw' && data.type === 'point') {
      this._cvlRememberChartPointCoordinates(data);
    }
    if (eventName === 'created') {
      this._cvlAddValuesLineToChart(chart);
      this._cvlAttachValuesColumnHoverListeners(chart);
      this._cvlShowValuesLineIfNeeded();
      this.set('_cvlChartCreated', true);
    }
  },

  /**
   * Persists coordinates of drawed point
   * @param {object} data chart event data
   */
  _cvlRememberChartPointCoordinates(data) {
    const _cvlPointsColumnXPosition = this.get('_cvlPointsColumnXPosition');
    const pointNode = data.element.getNode();
    const pointXCenter = (pointNode.x1.baseVal.value + pointNode.x2.baseVal.value) / 2;
    if (!_cvlPointsColumnXPosition[data.index]) {
      _cvlPointsColumnXPosition[data.index] = pointXCenter;
    }
  },

  /**
   * Adds values line to the chart svg
   * @param {object} chart chart object
   */
  _cvlAddValuesLineToChart(chart) {
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
   * Attaches listeners, that are responsible for monitoring actual hovered
   * values column
   * @param {object} chart chart object
   */
  _cvlAttachValuesColumnHoverListeners(chart) {
    $(chart.container).mousemove((event) => run(() => {
      safeExec(this, () => {
        this.set(
          '_cvlHoveredPointsColumnIndex',
          identifyHoveredValuesColumn(event, chart, this.get('_cvlPointsColumnXPosition'))
        );
        this._cvlShowValuesLineIfNeeded();
      });
    }))
    .mouseleave(() => run(() => {
      safeExec(this, () => {
        this.set('_cvlHoveredPointsColumnIndex', -1);
        this._cvlShowValuesLineIfNeeded();
      });
    }));
  },

  /**
   * Shows/hides value line
   */
  _cvlShowValuesLineIfNeeded() {
    const line = this.$('.ct-values-line');
    if (!line.length) {
      return;
    }
    const _hoveredPointsColumnIndex = this.get('_cvlHoveredPointsColumnIndex');
    this.$('.ct-point.ct-point-active').removeClass('ct-point-active');
    if (_hoveredPointsColumnIndex !== -1) {
      const x = this.get('_cvlPointsColumnXPosition')[_hoveredPointsColumnIndex];
      line.addClass('ct-values-line-active');
      line.attr('x1', x);
      line.attr('x2', x);
      this.$('.ct-series').each(function () {
        const point = $(this).find('.ct-point').get(_hoveredPointsColumnIndex);
        $(point).addClass('ct-point-active');
      });
    } else {
      line.removeClass('ct-values-line-active');
    }
  },
});
