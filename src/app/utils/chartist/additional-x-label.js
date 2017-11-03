/**
 * Plugin for Chartist which adds additional label on the right side of the x-axis.
 * 
 * Options:
 * - xOffsetMultiply - label will be moved right by xOffsetMultiply 
 * (default width of a label)
 * - insertBefore - label will be inserted before last label node
 *
 * @module utils/chartist/additional-x-label
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */

export default function (options) {
  let defaultOptions = {
    xOffsetMultiply: 1,
    insertBefore: false,
  };
  options = Chartist.extend({}, defaultOptions, options);
  return (chart) => {
    chart.on('created', () => {
      let labelsNode = $(chart.svg._node).find('.ct-labels');
      let labels = labelsNode.find('.ct-label.ct-horizontal.ct-end');
      let lastLabelNode = labelsNode.find('.ct-label.ct-horizontal.ct-end').last().parent();
      let sourceLabelNode = lastLabelNode;
      if (labels.length > 1) {
        sourceLabelNode = $(labels[labels.length - 2]).parent();
      }

      let newLabelNode = sourceLabelNode.clone();
      newLabelNode.attr('x',
        parseFloat(lastLabelNode.attr('x')) + options.xOffsetMultiply * parseFloat(
          sourceLabelNode.attr('width'))
      );
      newLabelNode.find('span').text(chart.data.lastLabel);
      if (!options.insertBefore) {
        newLabelNode.insertAfter(lastLabelNode);
      } else {
        newLabelNode.insertBefore(lastLabelNode);
      }
    });
  };
}
