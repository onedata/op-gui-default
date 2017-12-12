/**
 * Part of transfer data that is updated frequently
 * 
 * @module models/transfer-current-stat
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
  timestamp: attr('number'),
  transferredBytes: attr('number'),
  transferredFiles: attr('number'),
  bytesPerSec: attr('object'),
  
  /**
   * One of:
   * - scheduled
   * - active
   * - finalizing
   * - skipped
   * - completed
   * - cancelled
   * - failed
   */
  status: attr('string'),
});
