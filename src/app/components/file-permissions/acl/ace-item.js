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
    {id: 'user', text: 'User'},
    {id: 'group', text: 'Group'},
    // Not implemented yet in backend
    // { id: 'owner', text: 'Owner'},
    // { id: 'everyone', text: 'Everyone'},
  ],

  // FIXME: translate
  typeItems: [
    {id: 'allow', text: 'Allow'},
    {id: 'deny', text: 'Deny'},
    {id: 'audit', text: 'Audit'}
  ],

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

  filePermissionKeys: [
    'read_object',
    'write_object',
    'append_data',
    'read_metadata',
    'write_metadata',
    'execute',
    'delete_object',
    'read_attributes',
    'write_attributes',
    'delete',
    'read_acl',
    'write_acl',
    'write_owner'
  ],

  dirPermissionKeys: [
    'list_container',
    'add_object',
    'add_subcontainer',
    'read_metadata',
    'write_metadata',
    'traverse_container',
    'delete_subcontainer',
    'read_attributes',
    'write_attributes',
    'delete',
    'read_acl',
    'write_acl',
    'write_owner'
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
