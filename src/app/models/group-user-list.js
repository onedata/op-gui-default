import DS from 'ember-data';

const {
  belongsTo,
  hasMany
} = DS;

/**
 * FIXME
 * @module models/group-user-list
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  group: belongsTo('group', { async: true }),
  permissions: hasMany('group-user-permission', { async: true }),
});
