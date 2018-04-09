/**
 * Searches for appropriate column index according to mouse and columns positions.
 * 
 * @module utils/chartist/identify-hovered-values-column
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import $ from 'jquery';

/**
 * @param {MouseEvent} event 
 * @param {object} chart Chartist object
 * @param {Array<number>} _pointsColumnXPosition Array of columns positions
 * @return {number} positive index if column has been found, -1 otherwise
 */
export default function (event, chart, _pointsColumnXPosition) {
  const chartContainer = $(chart.container);
  const mouseX = event.pageX - chartContainer.offset().left;
  if (mouseX < _pointsColumnXPosition[0] ||
    mouseX > _pointsColumnXPosition[_pointsColumnXPosition.length - 1] ||
    _pointsColumnXPosition.length === 0) {
    return -1;
  } else {
    let targetIndex = 0;
    let targetIndexDistance = Math.abs(mouseX - _pointsColumnXPosition[0]);
    for (let i = 1; i < _pointsColumnXPosition.length; i++) {
      const distance = Math.abs(mouseX - _pointsColumnXPosition[i]);
      if (distance < targetIndexDistance) {
        targetIndex = i;
        targetIndexDistance = distance;
      }
    }
    return targetIndex;
  }
}
