import Ember from 'ember';

/**
 * A single row in permissions table - a view for space-user-permission or space-group-permission.
 * Each single permission type (eg. permission to view space) has its checkbox rendered.
 * @module components/permissions-table-row
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'div',
  classNames: ['one-tr', 'permissions-table-row'],
  classNameBindings: ['isActive:active'],

  // TODO custom ids for rows
  // setElementId: Ember.on('init', function() {
  //   this.set('elementId', `perm-row-${safeElementId(this.get('perm.id'))}-${this.get('type')}`);
  // }),

  /**
    Optional - if set, the type is used to generate element id
    Eg. it says, that the row represents user permissions
  */
  type: null,

  /**
    The subject of permissions {String}: space/group
  */
  subjectType: null,

  perm: Ember.computed.alias('permissions'),

  actions: {
    activate() {
      this.sendAction('activatePermissions', this.get('perm'));
    },

    /** Change state of single permission checkbox */
    togglePermission: function(permission, propertyName) {
      this.sendAction('togglePermission', permission, propertyName);
    },
  }
});
