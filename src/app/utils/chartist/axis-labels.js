/**
 * Plugin for Chartist which adds axis (x and y) labels.
 * 
 * Options:
 * - xLabel, yLabel - labels
 * - xLabelXOffset, xLabelYOffset, yLabelXOffset, yLabelYOffset - position
 * - yAlignment - 'left' (default) or 'right' - y axis label alignment
 * adjustments for x and y labels
 * 
 * Module imported from onedata-gui-common.
 *
 * @module utils/chartist/axis-labels
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */

export default function (options) {
  let defaultOptions = {
    xLabel: '',
    yLabel: '',
    xLabelXOffset: 15,
    xLabelYOffset: -20,
    yLabelXOffset: 20,
    yLabelYOffset: 20,
    yAlignment: 'left',
  };

  return (chart) => {
    chart.on('created', function () {
      options = Chartist.extend({}, defaultOptions, options);
      const dataAxisLabels = chart.data.axisLabels;
      if (dataAxisLabels) {
        options.xLabel = dataAxisLabels.xLabel;
        options.yLabel = dataAxisLabels.yLabel;
      }
      let svgNode = $(chart.svg._node);
      let axisLabelsGroup = chart.svg.elem('g', {}, 'ct-axis-labels');
      axisLabelsGroup.elem('text', {
        x: (options.yAlignment === 'right' ? -1 : 1) *
          (-svgNode.innerHeight() / 2 + options.yLabelYOffset),
        y: options.yAlignment === 'right' ?
          -svgNode.innerWidth() - options.yLabelXOffset : options.yLabelXOffset,
      }, 'ct-axis-y-label ' + options.yAlignment).text(options.yLabel);
      axisLabelsGroup.elem('text', {
        x: svgNode.innerWidth() / 2 + options.xLabelXOffset,
        y: svgNode.innerHeight() + options.xLabelYOffset,
      }, 'ct-axis-x-label').text(options.xLabel);
    });
  };
}
