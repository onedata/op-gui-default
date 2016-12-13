import Ember from 'ember';

function iconHTML(icon) {
  return Ember.HTMLBars.compile(`{{one-icon icon="${icon}"}}`);
} 

/**
 * A graphical representation of single ``AccessControlEntity``.
 *
 * @module components/ace-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  store: Ember.inject.service(),

  /**
   * @type AccessControlEntity
   */
  ace: null,

  file: null,

  /**
   * @type Array of {id: user_id, text: <displayed name>}
   */
  systemUsers: null,

  /**
   * @type Array of {id: group_id, text: <displayed name>}
   */
  systemGroups: null,

  subjectName: function() {
    let subjectsListProperty;
    let subjectIdProperty;
    switch (this.get('ace.subject')) {
      case 'user':
        subjectsListProperty = 'systemUsers';
        subjectIdProperty = 'ace.user';
        break;
      case 'group':
        subjectsListProperty = 'systemGroups';
        subjectIdProperty = 'ace.group';
        break;
      default:
        return null;
    }
    const subjectInfo = (this.get(subjectsListProperty) || [])
      .find(e => e.id === this.get(subjectIdProperty));
    return subjectInfo && subjectInfo.text;
  }.property('ace.subject', 'ace.user', 'ace.group'),

  /**
   * This should resolve subject type icon name for ace.type.
   * Currently icon names are the same as type name.
   */
  subjectTypeIcon: function() {
    return this.get('ace.subject');
  }.property('ace.subject'),

  subjectItems: function() {
    return [
      {id: 'user', text: iconHTML(['user'])},
      {id: 'group', text: iconHTML(['group'])},
      // Not implemented yet in backend
      // {id: 'owner', text: this.generateSubjectIconHtml('owner')},
      // {id: 'everyone', text: this.generateSubjectIconHtml('everyone')},
    ];
  }.property().readOnly(),

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
    switch (this.get('fileType')) {
      case 'file':
        return this.get('filePermissionKeys');
      case 'dir':
        return this.get('dirPermissionKeys');
      default:
        return null;
    }
  }.property('file.type').readOnly()
});
