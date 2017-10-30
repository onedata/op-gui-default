/**
 * Plugin for Chartist which sets custom colors for chartist-legend
 * 
 * Options:
 * * colors - array of colors for series
 * * styles - object of css styles for a legend item colored square
 *
 * @module utils/chartist/legend-colors
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default function (options) {
  return (chart) => {
    chart.on('created', () => {
      let legend = $(chart.container).find('.ct-legend');
      options.colors.forEach((color, index) => {
        let colorRect = $('<div class="custom-color"></div>').css({
          'position': 'absolute',
          'left': '0',
          'top': '0.2em',
          'width': '1em',
          'height': '1em',
          'border': '3px solid ' + color,
          'border-radius': '3px',
          'background-color': color,
        });
        if (options.styles) {
          colorRect.css(options.styles);
        }
        let series = legend.find('.ct-series-' + index);
        series.find('.custom-color').remove();
        legend.find('.ct-series-' + index).prepend(colorRect);
      });
    });
  };
}
