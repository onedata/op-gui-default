import DS from 'ember-data';

const {
  attr,
  hasMany
} = DS;

/**
 * A singleton model (only single record of this model is used at runtime).
 * This is a root of all model tree that user of GUI can get from backend.
 * 
 * @module models/user
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: attr('string'),
  defaultSpaceId: attr('string'),

  /*** Relations ***/

  spaces: hasMany('spaces', {
    async: true
  }),

  shares: hasMany('share', {
    async: true
  }),

  handleServices: hasMany('handle-service', {
    async: true
  }),
});
