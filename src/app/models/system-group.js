import DS from 'ember-data';

/**
 * A single Onedata group representation.
 * This can be any group that exists in Onedata no matter it is accessible by
 * current user. See also models/group
 *
 * @module models/system-group
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /** Name exposed in GUI */
  name: DS.attr('string'),

  /** Collection of space permissions models - each for single Space */
  spacesPermissions: DS.hasMany('spaceGroupPermission', {async: true}),

  groupsPermissions: DS.hasMany('groupGroupPermission', {async: true}),

});