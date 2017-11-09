/**
 * Plugin for Chartist which adds tooltip. For bar and line charts tooltip 
 * creates description based on chartist legend and values. For pie chart data for tooltip is 
 * taken from data.series.tooltipElements. For example:
 * ```
 * tooltipElements: [{
 *     name: 'prop1',
 *     value: '100',
 *   },
 *   {
 *     name: 'desc2',
 *     value: '23%',
 * }]
 * ```
 *
 * Options:
 * - chartType - type of the chart (bar, line, pie)
 * - rangeInTitle - takes two x axis labels instead of one to tooltip title
 * - renderAboveBarDescription - [bar chart only] if true, places tooltip 
 * above a text instead of bar
 * - topOffset - top offset of a tooltip
 * - valueSuffix - [bar/line chart only] suffix for tooltip entries (e.g. for units)
 * - roundValues - if true, values in tooltip will be rounded
 * 
 * @module utils/chartist/tooltip
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */
import _ from 'lodash';
import dynamicRound from 'op-worker-gui/utils/dynamic-round';

const TOOLTIP_HTML =
  `
  <div class="chart-tooltip">
    <div class="chart-tooltip-title"></div>
    <ul class="ct-legend">
    </ul>
    <div class="chart-tooltip-arrow"></div>
  </div>
`;

let chartsIndex = [];

export default function (options) {
  let defaultOptions = {
    chartType: 'bar',
    rangeInTitle: false,
    renderAboveBarDescription: false,
    topOffset: -10,
    valueSuffix: '',
    roundValues: true,
  };
  options = Chartist.extend({}, defaultOptions, options);

  return (chart) => {
    let tooltipNode,
      container = $(chart.container);

    let chartEntry = getChartRenderEntry(chart);

    let prepareTooltip = function (tooltipData, data) {
      // title
      let title = tooltipNode.find('.chart-tooltip-title');
      title.empty();
      title.append(chart.data.labels[data.index]);
      if (options.rangeInTitle) {
        if (chart.data.labels[data.index + 1]) {
          title.append(' - ' + chart.data.labels[data.index + 1]);
        } else if (chart.data.lastLabel) {
          title.append(' - ' + chart.data.lastLabel);
        }
      }

      // data series and values
      let ul = tooltipNode.find('.ct-legend');
      ul.empty();
      let suffix = options.valueSuffix ? ' ' + options.valueSuffix : '';
      tooltipData.forEach(d => {
        let value = d.value;
        if (options.roundValues && typeof value === 'number') {
          value = dynamicRound(value);
        }
        ul.append(`<li class="${d.className}">${d.name}: ${value + suffix}</li>`);
      });
    };

    chart.on('created', () => {
      if (!isPluginEnabled(chart)) {
        chartEntry.x = chartEntry.y = null;
        return;
      }
      tooltipNode = container.find('.chart-tooltip');
      if (tooltipNode.length === 0) {
        tooltipNode = $($.parseHTML(TOOLTIP_HTML));
        container.append(tooltipNode);
        tooltipNode.css('transform',
          `translateY(-100%) translateY(${options.topOffset}px) translateX(-50%)`);
      } else {
        if (chartEntry.x !== null) {
          let element = document.elementFromPoint(chartEntry.x, chartEntry.y);
          let elementIndex = chartEntry.showCallbacksTargets.indexOf(element);
          if (elementIndex > -1) {
            chartEntry.showCallbacks[elementIndex](chartEntry.x, chartEntry.y);
          } else {
            chartEntry.x = chartEntry.y = null;
            tooltipNode.removeClass('active');
          }
        } else {
          tooltipNode.removeClass('active');
        }
      }
      $(chart.svg.getNode()).mousemove((event) => {
        if (!$(event.target).parents('.ct-series').length) {
          tooltipNode.removeClass('active');
          chartEntry.x = chartEntry.y = null;
        }
      });
    });

    chart.on('draw', function (data) {
      if (!isPluginEnabled(chart)) {
        return;
      }
      let tooltipData = chart.data.series.map(s => ({
        className: s.className,
        name: s.name,
        value: s.data[data.index],
      }));

      if (data.type === 'bar' && options.chartType === 'bar') {
        let groupNode = $(data.group._node),
          barNode = $(data.element._node);
          
        barNode.mouseover(() => {
          let lastGroupNode = groupNode.parent().children().last();
          let lastGroupBar = $(lastGroupNode.children('line')[data.index]);

          // top position
          if (options.renderAboveBarDescription) {
            let sumLabel = $(lastGroupNode.children('text')[data.index]);
            tooltipNode.css('top', (sumLabel.offset().top - container.offset().top) +
              'px');
          } else {
            tooltipNode.css('top', (lastGroupBar.offset().top - container.offset()
              .top) + 'px');
          }
          // left position
          let rect = lastGroupBar[0].getBoundingClientRect();
          tooltipNode.css('left', (rect.left + rect.width / 2 - container.offset()
            .left) + 'px');

          prepareTooltip(tooltipData, data);

          tooltipNode.addClass('active');
        }).mouseout(() => {
          tooltipNode.removeClass('active');
        });
      }
      if (data.type === 'point' && options.chartType === 'line') {
        let groupNode = $(data.group._node),
          pointNode = $(data.element._node);
        tooltipData = data.series.tooltipElements && data.series.tooltipElements[data.index] ?
          data.series.tooltipElements[data.index] : tooltipData;
        pointNode.mouseover(() => {
          // top position
          let rect = pointNode[0].getBoundingClientRect();
          if (options.renderAboveBarDescription) {
            let sumLabel = $(groupNode.children('text')[data.index]);
            tooltipNode.css('top', (sumLabel.offset().top - container.offset().top) +
              'px');
          } else {
            tooltipNode.css('top', (rect.top - container.offset()
              .top) + 'px');
          }
          // left position
          tooltipNode.css('left', (rect.left + rect.width / 2 - container.offset()
            .left) + 'px');

          prepareTooltip(tooltipData, data);

          tooltipNode.addClass('active');
        }).mouseout(() => {
          tooltipNode.removeClass('active');
        });
      }
      if (data.type === 'slice' && options.chartType === 'pie') {
        data.series.tooltipElements.forEach(element => element.className =
          'no-padding');
        let tooltipData = data.series.tooltipElements;
        let sliceNode = $(data.element._node);
        let showTooltip = (x, y) => {
          tooltipNode.css('top', (y - container.offset().top - 10) +
            'px');
          tooltipNode.css('left', (x - container.offset()
            .left) + 'px');

          prepareTooltip(tooltipData, data);

          tooltipNode.addClass('active');
          chartEntry.x = x;
          chartEntry.y = y;
        };
        sliceNode.mousemove((event) => showTooltip(event.pageX, event.pageY))
          .mouseout(() => {
            tooltipNode.removeClass('active');
            chartEntry.x = chartEntry.y = null;
          });
        chartEntry.showCallbacksTargets.push(data.element.getNode());
        chartEntry.showCallbacks.push(showTooltip);
      }
    });
  };
}

function isPluginEnabled(chart) {
  return !chart.options.disabledPlugins ||
    chart.options.disabledPlugins.indexOf('tooltip') === -1;
}

function getChartRenderEntry(chart) {
  let node = chart.container;
  let chartRender = _.find(chartsIndex, { node });
  if (!chartRender) {
    chartRender = {
      node,
      x: null,
      y: null,
      showCallbacksTargets: [],
      showCallbacks: [],
    };
    // remove not existing charts renders
    chartsIndex = chartsIndex.filter((existingChartRender) => {
      return $.contains(document.documentElement, existingChartRender.node);
    });
    chartsIndex.push(chartRender);
  }
  return chartRender;
}
