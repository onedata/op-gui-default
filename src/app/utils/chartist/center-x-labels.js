/**
 * Plugin for Chartist which centers x labels.
 *
 * @module utils/chartist/center-x-labels
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default function () {
  return (chart) => {
    chart.on('created', () => {
      $(chart.container).find('.ct-labels .ct-label.ct-horizontal.ct-end')
        .parent()
        .each((index, element) => {
          let label = $(element);
          let width = parseFloat(label.attr('width'));
          label.css({'transform': `translateX(-${width / 2}px)`});
        });
    });
  };
}
