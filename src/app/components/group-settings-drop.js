import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

/**
 * Drop-right menu for single group, conaining i.a. rename group, remove group etc.
 * Component does not have groups manipulation logic - actions are sended to parent components or services.
 * @module components/group-settings-drop
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  commonModals: Ember.inject.service(),

  classNames: ['item-element', 'item-icon'],

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
        // TODO: this icons should be named "leave" in oneicons
        icon: 'leave-space',
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
        icon: 'space-add',
        label: i18n.t('components.groupsMenu.drop.createSpace'),
        action: 'requestSpaceCreation'
      },
      {
        icon: 'remove',
        label: i18n.t('components.groupsMenu.drop.remove'),
        action: 'removeGroup'
      },
    ].filter((item) => !item.disabled);
  }.property('group', 'group.parentGroups', 'group.parentGroups.length'),

  // TODO: deregister event from sidebar on willDestroyElement
  // maybe use: this.on('willDestroyElement', () => { sidebar.off(...) } ) etc.
  didInsertElement() {
    let sidebar = $('.secondary-sidebar');
    let drop = this.$().find('.dropdown-menu');
    let updater = bindFloater(drop, null, {
      offsetX: 8
    });
    sidebar.on('scroll', updater);
    drop.on('mouseover', updater);

    // a hack to update drop position after group menu expand
    // this hack is probably not needed anymore, because groups menu doesn't expand
    // on settings icon click - but we leave it "just in case"
    drop.closest('.settings-dropdown').on('click', function() {
      window.setTimeout(() => {
        updater();
      }, 50);
    });
  },

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
      this.get('commonModals').openInfoModal(
        i18n.t('common.featureNotSupportedShort'),
        i18n.t('common.featureNotSupportedLong')
      );
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

    requestSpaceCreation() {
      this.get('commonModals').openModal('token', {
        type: 'requestSpaceCreation',
        funArgs: [this.get('group.id')]
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
