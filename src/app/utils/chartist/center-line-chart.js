/**
 * Plugin for Chartist which centers charts (moves chart by a half of width of a column), 
 * that have one more column than there is available data.
 *
 * @module utils/chartist/center-line-chart
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default function () {
  return (chart) => {
    chart.on('created', function () {
      let series = $(chart.svg._node).find('.ct-series');
      let points = series.first().find('.ct-point');
      let deltaX = $(points[1]).attr('x1') - points.first().attr('x1');
      series.attr('transform', `translate(${deltaX / 2})`);
    });
  };
}
