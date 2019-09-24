import Ember from 'ember';

const {
  String: {
    htmlSafe
  },
  inject,
  computed
} = Ember;

/**
 * @return {SafeString} a HTML that should display icon as in one-icon
 */
function iconHTML(icon) {
  return htmlSafe(`<span class="one-icon oneicon oneicon-${icon}"></span>`);
}

/**
 * A graphical representation of single ``AccessControlEntity``.
 *
 * @module components/ace-item
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  store: inject.service(),

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

  types: ['allow', 'deny'],

  /**
   * Permission subject name for read-only entries. Eg. user name.
   * @type {computed<string>}
   */
  subjectName: computed(
    'ace.{subject,user,group}',
    'systemGroups.@each.id',
    'systemUsers.@each.id',
    function subjectName() {
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
    }
  ),

  /**
   * This should resolve subject type icon name for ace.type.
   * Currently icon names are the same as type name.
   */
  subjectTypeIcon: computed.alias('ace.subject'),

  subjectItems: computed(function() {
    return [
      {id: 'user', text: iconHTML('user')},
      {id: 'group', text: iconHTML('group')},
    ];
  }).readOnly(),

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
    'perm_read_metadata',
    'perm_write_metadata',
    'perm_read_attributes',
    'perm_write_attributes',
    'perm_delete',
    'perm_read_acl',
    'perm_write_acl',
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
    'perm_delete_child',
    'perm_read_attributes',
    'perm_write_attributes',
    'perm_delete',
    'perm_read_acl',
    'perm_write_acl',
  ],

  permissionKeys: computed('file.type', function() {
    switch (this.get('fileType')) {
      case 'file':
        return this.get('filePermissionKeys');
      case 'dir':
        return this.get('dirPermissionKeys');
      default:
        return null;
    }
  }).readOnly()
});
