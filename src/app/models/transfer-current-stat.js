/**
 * Part of transfer data that is updated frequently
 * 
 * @module models/transfer-current-stat
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
  timestamp: attr('number'),
  replicatedBytes: attr('number'),
  replicatedFiles: attr('number'),
  invalidatedFiles: attr('number'),
  
  /**
   * One of:
   * - scheduled
   * - replicating
   * - invalidating
   * - aborting
   * - skipped
   * - completed
   * - cancelled
   * - failed
   */
  status: attr('string'),
});
