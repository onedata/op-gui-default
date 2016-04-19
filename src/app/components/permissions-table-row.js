import Ember from 'ember';
import safeElementId from '../utils/safe-element-id';

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
