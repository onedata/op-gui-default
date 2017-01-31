import DS from 'ember-data';

const {
  belongsTo,
  hasMany
} = DS;

/**
 * Model with group permissions list for group.
 * @module models/group-group-list
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  space: belongsTo('space', { async: true }),
  permissions: hasMany('space-user-permission', { async: true, inverse: null }),
});
