/**
 * A function, which generates `n` colors using `baseColors` as a source data
 * for calculation. If `baseColors` argument is not provided, default application
 * color scheme is used.
 * 
 * @module utils/generate-colors
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import _ from 'lodash';
import Color from 'npm:color';

// different than in onedata-gui-common
const colors = {
  'azure': '#3EA5F9',
  'violet': '#8549D4',
  'yellow': '#F7AA04',
  'blue': '#3B5998',
};
const COLOR_CHANGE_PADDING = 0.2;

export default function generateColors(n, baseColors) {
  if (!baseColors || baseColors.length === 0) {
    baseColors = _.values(colors);
  }
  let nPerColor = Math.ceil(n / baseColors.length);
  let generatedColors = [];
  let colorChangeStep = (1 - COLOR_CHANGE_PADDING) / nPerColor;
  for (let i = 0; i < n; i++) {
    let baseColor = baseColors[i % baseColors.length];
    let inColorIndex = Math.floor(i / baseColors.length);
    let stepMultiply = (inColorIndex % 2 === 0) ?
      -inColorIndex / 2 : Math.floor(inColorIndex / 2) + 1;
    let change = Math.abs(stepMultiply * colorChangeStep);
    let color = new Color(baseColor);
    let newColor = stepMultiply < 0 ? color.darken(change) : color.lighten(change);
    generatedColors.push(newColor.hex());
  }
  return generatedColors;
}
