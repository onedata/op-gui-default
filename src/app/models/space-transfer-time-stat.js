/**
 * Summarized transfer speed statistics to given time span (type)
 * 
 * @module models/space-transfer-time-stat
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
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
   * Keys: destination provider Id
   * Values: array of speeds (B/s)
   *  - [0] value is an average speed sum between transfer
   *    startTime to start of first time slot
   *  - [1..length-2] values are average speeds sums in time slots
   * -  [length-1] value is average speed sum between end of last time slot
   *    to timestamp
   */
  statsIn: attr('object'),

  /**
   * Keys: source provider Id
   * Values: like in `statsIn`
   */
  statsOut: attr('object'),
});
