import DS from 'ember-data';

/**
 * A configuration of a space - entry point for all options
 * that can be reached from "spaces" button in primary sidebar.
 *
 * @module models/space
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /** User specified name of space that will be exposed in GUI */
  name: DS.attr('string'),
  /** Collection of users permissions - effectively all rows in permissions table */
  userPermissions: DS.hasMany('spaceUserPermission', {async: true}),
  /** Collection of group permissions - effectively all rows in permissions table */
  groupPermissions: DS.hasMany('spaceGroupPermission', {async: true}),
  /** Whether user specified this space as default */
  isDefault: DS.attr('boolean', {defaultValue: false}),

  groups: DS.hasMany('group', {async: true}),

// TODO: currently not used - use list Order in templates
  /** An absolute position on list */
  listOrder: DS.attr('number'),

  dataSpace: DS.belongsTo('dataSpace', {async: true}),

  hasViewPrivilege: DS.attr('boolean'),

  save() {
    const p = this._super(...arguments);
    p.then(() => {
      this.get('dataSpace').then(s => s.update());
    });
    return p;
  }
});
