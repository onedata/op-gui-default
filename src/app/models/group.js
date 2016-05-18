import DS from 'ember-data';

// FIXME: documentation do not explain what is difference between group and system-group

/**
 * A group in system - model for groups/ routes.
 *
 * @module models/group
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /** User specified name of space that will be exposed in GUI */
  name: DS.attr('string'),
  /** Collection of users permissions - each will be a row in permissions table */
  userPermissions: DS.hasMany('groupUserPermission', {async: true}),
  /** Collection of group permissions - each will be a row in permissions table */
  groupPermissions: DS.hasMany('groupGroupPermission', {async: true}),

  // TODO: this property is currently not supported in backend
  spaces: DS.hasMany('space', {async: true}),

  parentGroups: DS.hasMany('group', {async: true}),

  // TODO: currently not used - use list Order in templates
  /** An absolute position on list */
  listOrder: DS.attr('number'),
});
