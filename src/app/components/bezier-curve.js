/**
 * A component that creates symmetric Bezier curve using to 
 * two points and curveFactor.
 * 
 * Module imported from onedata-gui-common.
 * 
 * @module components/bezier-curve
 * @author Michal Borzecki, Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

const {
  sqrt,
  abs,
} = Math;

export default Component.extend({
  tagName: 'path',
  classNames: ['bezier-curve'],
  attributeBindings: ['d'],

  /**
   * Start point x coordinate
   * @type {number}
   */
  x1: 0,

  /**
   * Start point y coordinate
   * @type {number}
   */
  y1: 0,

  /**
   * End point x coordinate
   * @type {number}
   */
  x2: 0,

  /**
   * End point y coordinate
   * @type {number}
   */
  y2: 0,

  /**
   * Curve factor. Large values (positive or negative) will flatten curve.
   * If curveFactor < 0, then curve is 'upside down'.
   * Warning: Must be different from 0!
   * 
   * Default value `sqrt(3)` means, that edge point, middle point
   * of the bezier curve and the middle point of the specified section 
   * will create triangle 30, 60, 90 degrees.
   * 
   * @type {number}
   */
  curveFactor: sqrt(3),

  /**
   * Bezier curve definition.
   * @type {Ember.ComputedProperty<string>}
   */
  d: computed('x1', 'y1', 'x2', 'y2', 'curveFactor', function () {
    const {
      x1,
      x2,
      y1,
      y2,
      curveFactor,
    } = this.getProperties('x1', 'x2', 'y1', 'y2', 'curveFactor');

    // using calculationsDelta to avoid dividing by 0
    const calculationsDelta = 0.01;
    const deltaX = x2 - x1;
    const deltaY = y1 - y2;
    const xyLength = sectionLength(deltaX, deltaY);
    let vector, resultVector;

    if (xyLength < calculationsDelta) {
      resultVector = [0, 0];
    } else {
      if (abs(deltaY) < calculationsDelta) {
        // line is horizontal, so vector should be vertical
        vector = [0, 1];
      } else if (abs(deltaX) < calculationsDelta) {
        // line is vertical, so vector should be horizontal
        vector = [1, 0];
      } else {
        // searching for vector using 'a' constant of a linear function (line)
        const a = deltaY / deltaX;
        const aPrim = -1 / a;
        vector = a < 0 ? [1, aPrim] : [-1, -aPrim];
      }
      const initialVectorLength = sectionLength(vector[0], vector[1]);
      const scaleFactor = xyLength / (2 * curveFactor * initialVectorLength);

      resultVector = [scaleFactor * vector[0], scaleFactor * vector[1]];
    }
    // translate middle point using resultVector
    const xi = (x2 + x1) / 2 + resultVector[0];
    const yi = (y2 + y1) / 2 - resultVector[1];

    return `M ${x1} ${y1} Q ${xi} ${yi}, ${x2} ${y2}`;
  }),
});

function sectionLength(deltaX, deltaY) {
  return sqrt(deltaX * deltaX + deltaY * deltaY);
}
