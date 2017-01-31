import Ember from 'ember';
import SettingsDropMixin from 'op-worker-gui/mixins/components/settings-drop';

/**
 * Drop-right menu for single group, conaining i.a. rename group, remove group etc.
 * Component does not have groups manipulation logic - actions are sent to parent components or services.
 * @module components/group-settings-drop
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(SettingsDropMixin, {
  commonModals: Ember.inject.service(),
  messageBox: Ember.inject.service(),

  /**
    Items in "group settings" dropright menu
    Each item has properties:
    ```
    {
      icon: <string> - name of oneicon,
      label: <string> - label to show in menu (please use i18n service),
      action: <string> - name of action of this component
    }
    ```
  */
  menuItemsCurrentUser: function() {
    let i18n = this.get('i18n');
    return [
      {
        // the same icon as in leave-group,
        // TODO: this icons should be named "leave" in oneicons
        icon: 'leave-space',
        label: i18n.t('components.groupsMenu.drop.leave'),
        action: 'leaveGroup'
      },
    ].filter((item) => !item.disabled);
  }.property(),

  menuItemsGroup: function() {
    let i18n = this.get('i18n');
    return [
      {
        icon: 'rename',
        label: i18n.t('components.groupsMenu.drop.rename'),
        action: 'renameGroup'
      },
      {
        icon: 'user-add',
        label: i18n.t('components.groupsMenu.drop.inviteUser'),
        action: 'inviteUser'
      },
      {
        icon: 'group-invite',
        label: i18n.t('components.groupsMenu.drop.inviteGroup'),
        action: 'inviteGroup'
      },
      {
        icon: 'join',
        label: i18n.t('components.groupsMenu.drop.joinAsSubgroup'),
        action: 'joinAsSubgroup'
      },
      {
        // the same icon as in leave-group,
        icon: 'group-leave-group',
        label: i18n.t('components.groupsMenu.drop.leaveParentGroup'),
        action: 'leaveParentGroup',
        disabled: (!this.get('group.parentGroups') || this.get('group.parentGroups.length') === 0)
      },
      {
        icon: 'space-join',
        label: i18n.t('components.groupsMenu.drop.joinSpace'),
        action: 'joinSpace'
      },
      {
        icon: 'remove',
        label: i18n.t('components.groupsMenu.drop.remove'),
        action: 'removeGroup'
      },
    ].filter((item) => !item.disabled);
  }.property('group', 'group.parentGroups', 'group.parentGroups.length'),

  actions: {
    leaveGroup() {
      this.sendAction('openSettingsModal', 'leave', this.get('group'));
    },

    leaveParentGroup() {
      this.sendAction('openSettingsModal', 'leaveParentGroup', this.get('group'));
    },

    renameGroup() {
      this.sendAction('openSettingsModal', 'rename', this.get('group'));
    },

    removeGroup() {
      let i18n = this.get('i18n');
      this.get('messageBox').open({
        title: i18n.t('common.featureNotSupportedShort'),
        type: 'warning',
        allowClose: true,
        message: i18n.t('common.featureNotSupportedLong')
      });

      // TODO: remove function currently disabled
      // this.sendAction('openSettingsModal', 'remove', this.get('group'));
    },


    inviteGroup() {
      this.get('commonModals').openModal('token', {
        type: 'groupJoinGroup',
        funArgs: [this.get('group.id')],
      });
    },

    inviteUser() {
      this.get('commonModals').openModal('token', {
        type: 'userJoinGroup',
        funArgs: [this.get('group.id')],
      });
    },

    joinSpace() {
      this.sendAction('openSettingsModal', 'joinSpace', this.get('group'));
    },

    joinAsSubgroup() {
      this.sendAction('openSettingsModal', 'joinAsSubgroup', this.get('group'));
    },
  }
});
