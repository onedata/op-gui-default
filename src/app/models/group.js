import DS from 'ember-data';

/**
 * A group in system - model for groups/ routes.
 * This must be a group that is accessible for current user.
 * See also models/system-group
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

  parentGroups: DS.hasMany('group', {async: true, inverse: 'childGroups'}),
  childGroups: DS.hasMany('group', {async: true, inverse: 'parentGroups'}),

  // TODO: currently not used - use list Order in templates
  /** An absolute position on list */
  listOrder: DS.attr('number'),

  hasViewPrivilege: DS.attr('boolean'),
});
