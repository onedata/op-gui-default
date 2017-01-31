import DS from 'ember-data';

const {
  belongsTo,
  hasMany
} = DS;

/**
 * Model with user permissions list for group.
 * @module models/group-group-list
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  group: belongsTo('group', { async: true }),
  permissions: hasMany('group-user-permission', { async: true }),
});
