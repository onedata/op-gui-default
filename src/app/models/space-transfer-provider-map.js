/**
 * Contains transfers mapping: sourceProvider -> array of destinationProvider
 * 
 * @module models/space-transfer-provider-map
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

const {
  Model,
  attr,
} = DS;

export default Model.extend({
  /**
   * Keys: source provider Id
   * Values: array of destination providers ids
   */
  mapping: attr('object'),
});
