/**
 * Plugin for Chartist which shorten horizontal grid to the specified height and place it in the middle of the x axis.
 *
 * Options:
 * - height - height of the horizontal grid
 * 
 * @module utils/chartist/short-horizontal-grid
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/* global Chartist */

export default function (options) {
  return (chart) => {
    let defaultOptions = {
      height: 6,
    };
    options = Chartist.extend({}, defaultOptions, options);

    chart.on('created', () => {
      let gridLines = $(chart.container).find('.ct-grid.ct-horizontal');
      let oldY2 = parseFloat(gridLines.first().attr('y2'));
      gridLines.each((index, element) => {
        $(element).attr('y1', oldY2 - options.height / 2);
        $(element).attr('y2', oldY2 + options.height / 2);
      });
    });
  };
}
