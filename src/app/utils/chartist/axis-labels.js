/**
 * Plugin for Chartist which adds axis (x and y) labels.
 * 
 * Options:
 * - xLabel, yLabel - labels
 * - xLabelXOffset, xLabelYOffset, yLabelXOffset, yLabelYOffset - position 
 * adjustments for x and y labels
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
  };
  options = Chartist.extend({}, defaultOptions, options);

  return (chart) => {
    chart.on('created', function () {
      let svgNode = $(chart.svg._node);
      let axisLabelsGroup = chart.svg.elem('g', {}, 'ct-axis-labels');
      axisLabelsGroup.elem('text', {
        x: -svgNode.innerHeight() / 2 + options.yLabelYOffset,
        y: options.yLabelXOffset,
      }, 'ct-axis-y-label').text(options.yLabel);
      axisLabelsGroup.elem('text', {
        x: svgNode.innerWidth() / 2 + options.xLabelXOffset,
        y: svgNode.innerHeight() + options.xLabelYOffset,
      }, 'ct-axis-x-label').text(options.xLabel);
    });
  };
}
