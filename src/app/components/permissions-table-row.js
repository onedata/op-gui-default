import Ember from 'ember';
import safeElementId from '../utils/safe-element-id';

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
  setElementId: Ember.on('init', function() {
    this.set('elementId', `perm-row-${safeElementId(this.get('perm.id'))}`);
  }),

  perm: Ember.computed.alias('permissions'),

  click() {
    this.sendAction('activatePermissions', this.get('perm'));
  },

  actions: {
    /** Change state of single permission checkbox */
    togglePermission: function(permission, propertyName) {
      this.sendAction('togglePermission', permission, propertyName);
    },
  }
});
