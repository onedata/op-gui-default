import DS from 'ember-data';

/**
 * A single Onedata user representation
 * @module models/system-user
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /** Name exposed in GUI */
  name: DS.attr('string'),

  /** Collection of space permissions models - each for single Space */
  spacesPermissions: DS.hasMany('spaceUserPermission', {async: true}),

  groupsPermissions: DS.hasMany('groupUserPermission', {async: true})
});
