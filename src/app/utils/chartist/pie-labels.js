/**
 * A plugin for Chartist which adds pie chart labels.
 * 
 * Options:
 * * lineTextMargin - margin between text and line,
 * * lineLength - horizontal (next to text) line length,
 * * linePointerLength - diagonal (pointer to slice) line length,
 * * hideLabelThresholdPercent - percent below which label is hidden.
 * 
 * Both lineLength and linePointerLength can be a number (means px),
 * or a percent string (relative to radius size).
 * 
 * Used css classes:
 * * ct-pie-label-line - for lines,
 * * ct-pie-label-text - for text (both top and bottom),
 * * ct-pie-label-text-top - for top text
 * * ct-pie-label-text-bottom - for bottom text
 *
 * @module utils/chartist/pie-labels
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */

import _ from 'lodash';

const DEFAULT_LINE_LENGTH = '100';
const DEFAULT_LINE_POINTER_LENGTH = '50%';

let chartsIndex = [];

export default function (options) {
  let defaultOptions = {
    lineTextMargin: 3,
    lineLength: DEFAULT_LINE_LENGTH,
    linePointerLength: DEFAULT_LINE_POINTER_LENGTH,
    hideLabelThresholdPercent: 15
  };
  options = Chartist.extend({}, defaultOptions, options);
  return (chart) => {
    chart.on('draw', (data) => {
      if (!isPluginEnabled(chart)) {
        return;
      }
      if (data.type === 'slice') {
        if (!chart.data.pieLabels[data.index]) {
          return;
        }
        let svg = chart.svg;
        let labelsGroup = getLabelsGroup(svg);
        let additionalClasses = chart.data.pieLabels[data.index].className;
        let labelGroup = svg.elem('g', {},
          'ct-pie-label ' + (additionalClasses ? additionalClasses : ''));
        labelsGroup.append(labelGroup);

        let radiansAverage = ((data.startAngle + data.endAngle) * Math.PI) / 360;
        let distance = data.radius;
        let x = data.center.x + distance * Math.sin(radiansAverage);
        let y = data.center.y - distance * Math.cos(radiansAverage);

        let lineLength = normalizeLength(
          options.lineLength,
          data.radius,
          DEFAULT_LINE_LENGTH
        );
        let horizDirection = x > data.center.x ? 1 : -1;
        let vertDirection = y > data.center.y ? 1 : -1;
        let pointerLineLength = normalizeLength(
          options.linePointerLength,
          data.radius,
          DEFAULT_LINE_POINTER_LENGTH
        );

        let pointerLineLengthX = pointerLineLength / Math.SQRT2;
        let lineX2a = x + horizDirection * pointerLineLengthX;
        let lineY2a = y + vertDirection * pointerLineLengthX;
        let lineX3a = lineX2a + horizDirection * lineLength;
        let lineY3a = lineY2a;
        let lineAttributes = {
          d: `M ${x} ${y} L ${lineX2a} ${lineY2a} L ${lineX3a} ${lineY3a}`,
          fill: 'none',
        };

        let line = labelGroup.elem('path', lineAttributes, 'ct-pie-label-line');
        labelGroup.append(line);

        addText(chart, data, options, labelGroup,
          lineX3a, lineY3a, horizDirection, lineLength);
        autohideLabel(data, options, labelGroup);
        chart.eventEmitter.emit('draw', {
          type: 'pie-label',
          element: labelGroup,
          group: labelGroup,
          index: data.index,
        });
      }
    });
    chart.on('created', () => {
      let chartRender = getChartRenderEntry(chart);
      let lastActiveTooltipTarget = _.find(
        chartRender.oldTooltipTargets,
        (target) => !!$(target).attr('aria-describedby')
      );
      if (lastActiveTooltipTarget) {
        lastActiveTooltipTarget = $(lastActiveTooltipTarget);
        let lastTooltipId = lastActiveTooltipTarget.attr('aria-describedby');
        if ($('#' + lastTooltipId).hasClass('in')) {
          // tooltip was active while rerender. Try to activate tooltip, which is 
          // rendered exactly in the same place
          let dx = lastActiveTooltipTarget.attr('dx');
          let dy = lastActiveTooltipTarget.attr('dy');
          let newTarget = $(chart.container).find(`text[dx="${dx}"][dy="${dy}"]`);
          newTarget.tooltip('show');
        }
      }
      // remove all tooltips from the previous render
      chartRender.oldTooltipTargets.forEach((target) =>
        $('#' + $(target).attr('aria-describedby')).remove()
      );
      chartRender.oldTooltipTargets = chartRender.actualTooltipTargets;
      chartRender.actualTooltipTargets = [];
    });
  };
}

function isPluginEnabled(chart) {
  return !chart.options.disabledPlugins ||
    chart.options.disabledPlugins.indexOf('pieLabels') === -1;
}

function getLabelsGroup(svg) {
  let labelsGroup = svg.querySelector('.ct-pie-labels');
  if (!labelsGroup) {
    labelsGroup = svg.elem('g', {}, 'ct-pie-labels');
    svg.append(labelsGroup);
  }
  return labelsGroup;
}

function normalizeLength(length, relativeLength, defaultValue) {
  if (typeof length === 'string') {
    length = length.trim();
  } else if (typeof length === 'number') {
    length = String(length);
  } else {
    length = String(defaultValue);
  }
  if (length[length.length - 1] === '%') {
    return relativeLength * (parseFloat(length) / 100);
  } else {
    return parseFloat(length);
  }
}

function addText(
  chart, data, options, labelGroup, x, y, horizDirection, width
) {
  let textAttributes = {
    'dx': x,
    'dy': y - options.lineTextMargin,
    'text-anchor': horizDirection > 0 ? 'end' : 'start',
    'style': 'dominant-baseline: text-after-edge;',
  };

  let textElement = labelGroup.elem(
    'text',
    textAttributes,
    'ct-pie-label-text ct-pie-label-text-top'
  );
  clipText(textElement, chart.data.pieLabels[data.index].topText, width, chart);
  labelGroup.append(textElement);

  textAttributes.dy = y + options.lineTextMargin;
  textAttributes.style = 'dominant-baseline: text-before-edge;';

  textElement = labelGroup.elem(
    'text',
    textAttributes,
    'ct-pie-label-text ct-pie-label-text-bottom'
  );
  clipText(textElement, chart.data.pieLabels[data.index].bottomText, width, chart);
  labelGroup.append(textElement);
}

function clipText(textElement, text, width, chart) {
  // some padding for readability
  width -= 5;
  textElement.text(text);
  if (textElement.width() <= width || text.length <= 1) {
    return;
  } else {
    // binary search of proper text length
    let upperIndex = text.length - 1;
    let lowerIndex = 1;
    let clippedText;
    while (lowerIndex !== upperIndex) {
      let newIndex = Math.ceil((upperIndex + lowerIndex) / 2);
      clippedText = text.substring(0, newIndex) + '...';
      textElement.empty().text(clippedText);
      if (textElement.width() > width) {
        upperIndex = newIndex - 1;
      } else {
        lowerIndex = newIndex;
      }
    }

    let chartRender = getChartRenderEntry(chart);
    chartRender.actualTooltipTargets.push(textElement.getNode());
    $(textElement.getNode()).tooltip({
      container: 'body',
      title: text,
      animation: false,
      placement: 'auto top',
    });
  }
}

function autohideLabel(data, options, labelGroup) {
  let degreesDelta = data.endAngle - data.startAngle;
  if (degreesDelta < options.hideLabelThresholdPercent * 3.6) {
    let $labelGroup = $(labelGroup.getNode());
    let $slice = $(data.element.getNode());
    let showLabel = () => $labelGroup.css('display', 'initial');
    let hideLabel = () => $labelGroup.css('display', 'none');
    hideLabel();
    $slice.mouseover(showLabel).mouseout(hideLabel);
    $labelGroup.mouseover(showLabel).mouseout(hideLabel);
  }
}

function getChartRenderEntry(chart) {
  let node = chart.container;
  let chartRender = _.find(chartsIndex, { node });
  if (!chartRender) {
    chartRender = {
      node,
      oldTooltipTargets: [],
      actualTooltipTargets: [],
    };
    // remove not existing charts renders
    chartsIndex = chartsIndex.filter((existingChartRender) => {
      return $.contains(document.documentElement, existingChartRender.node);
    });
    chartsIndex.push(chartRender);
  }
  return chartRender;
}
