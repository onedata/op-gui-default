/**
 * Transfer speed statistics to given time span (type)
 * 
 * @module models/transfer-time-stat
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

const {
  Model,
  attr,
} = DS;

export default Model.extend({
  /**
   * Unix timestamp of last stat value
   */
  timestamp: attr('number'),
  
  /**
   * Timespan of statistics, one of: minute, hour, day, month (30 days)
   */
  type: attr('string'),
  
  /**
   * Keys: source provider Id
   * Values: array of speeds (B/s)
   *  - [0] value is an average speed between transfer
   *    startTime to start of first time slot
   *  - [1..length-2] values are average speeds in time slots
   * -  [length-1] value is average speed between end of last time slot
   *    to timestamp
   */
  stats: attr('object'),
});
