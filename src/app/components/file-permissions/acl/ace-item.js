import Ember from 'ember';

export default Ember.Component.extend({
  store: Ember.inject.service(),

  /**
   * @type AccessControlEntity
   */
  ace: null,

  file: null,

  // To inject:
  // systemUsers: null,
  // systemGroups: null,

  // FIXME: translate
  // FIXME: icon
  subjectItems: [
    {id: 'user', text: new Ember.Handlebars.SafeString('<span class="oneicon oneicon-user"></span>')},
    {id: 'group', text: new Ember.Handlebars.SafeString('<span class="oneicon oneicon-group"></span>')},
    // Not implemented yet in backend
    // { id: 'owner', text: 'Owner'},
    // { id: 'everyone', text: 'Everyone'},
  ],

  types: ['allow', 'deny'],

  // TODO: these actions can be probably invoked as: (action 'moveUp' ace) in view
  actions: {
    removeSelf() {
      this.sendAction('removeAceItemAction', this.get('ace'));
    },

    moveUp() {
      this.sendAction('moveUpAction', this.get('ace'));
    },

    moveDown() {
      this.sendAction('moveDownAction', this.get('ace'));
    }
  },

  /**
   * Permissions listed here will be presented to change for directory.
   */
  filePermissionKeys: [
    'perm_read_object',
    'perm_write_object',
    'perm_append_data',
    'perm_read_metadata',
    'perm_write_metadata',
    'perm_execute',
    'perm_delete_object',
    'perm_read_attributes',
    'perm_write_attributes',
    'perm_delete',
    'perm_read_acl',
    'perm_write_acl',
    'perm_write_owner'
  ],

  /**
   * Permissions listed here will be presented to change for directory.
   */
  dirPermissionKeys: [
    'perm_list_container',
    'perm_add_object',
    'perm_add_subcontainer',
    'perm_read_metadata',
    'perm_write_metadata',
    'perm_traverse_container',
    'perm_delete_subcontainer',
    'perm_read_attributes',
    'perm_write_attributes',
    'perm_delete',
    'perm_read_acl',
    'perm_write_acl',
    'perm_write_owner'
  ],

  permissionKeys: function() {
    switch (this.get('file.type')) {
      case 'file':
        return this.get('filePermissionKeys');
      case 'dir':
        return this.get('dirPermissionKeys');
      default:
        return null;
    }
  }.property('file.type').readOnly()
});
