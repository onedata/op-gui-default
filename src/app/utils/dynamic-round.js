/**
 * Produces string describing provided value with taking into account minimal value
 *
 * For example if precision is 3, max 3 digits after point are generated:
 * - 0.0003 => "< 0.001"
 * - 1.23 => "1.23"
 *
 * @module utils/dynamic-round
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import round10 from 'npm:round10';

export default function dynamicRound(value, precision = 1) {
  let isNumberLowerThanPrecision =
    Math.floor(Math.abs(value) * Math.pow(10, precision)) === 0;
  if (isNumberLowerThanPrecision && value !== 0) {
    return `< ${Math.pow(10, -precision)}`;
  } else {
    return round10.round10(value, -precision).toString();
  }
}
