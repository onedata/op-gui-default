import DS from 'ember-data';

const {
  belongsTo,
  hasMany
} = DS;

/**
 * FIXME
 * @module models/space-user-list
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  space: belongsTo('space', { async: true }),
  permissions: hasMany('spaceUserPermission', { async: true, inverse: null }),
});
