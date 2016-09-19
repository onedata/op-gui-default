import Ember from 'ember';
import SettingsDropMixin from 'op-worker-gui/mixins/components/settings-drop';

/**
 * Drop-right menu for single space, conaining i.a. rename space, remove space etc.
 * Component does not have spaces manipulation logic - actions are sent to parent components or services.
 * @module components/space-settings-drop
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(SettingsDropMixin, {
  commonModals: Ember.inject.service(),
  messageBox: Ember.inject.service(),

  /**
    Items in "space settings" dropright menu
    Each item has properties:
    ```
    {
      icon: <string> - name of oneicon,
      label: <string> - label to show in menu (please use i18n service),
      action: <string> - name of action of this component
    }
    ```
  */
  menuItems: function() {
    let i18n = this.get('i18n');
    return [
      {
        icon: 'home',
        label: i18n.t('components.spacesMenu.drop.setHome'),
        action: 'setAsHome'
      },
      {
        icon: 'leave-space',
        label: i18n.t('components.spacesMenu.drop.leave'),
        action: 'leaveSpace'
      },
      {
        icon: 'rename',
        label: i18n.t('components.spacesMenu.drop.rename'),
        action: 'renameSpace'
      },
      // TODO: disabled in beta4
      // {
      //   icon: 'remove',
      //   label: i18n.t('components.spacesMenu.drop.remove'),
      //   action: 'removeSpace'
      // },
      {
        icon: 'user-add',
        label: i18n.t('components.spacesMenu.drop.inviteUser'),
        action: 'inviteUser'
      },
      {
        icon: 'group-invite',
        label: i18n.t('components.spacesMenu.drop.inviteGroup'),
        action: 'inviteGroup'
      },
      {
        icon: 'support',
        label: i18n.t('components.spacesMenu.drop.getSupport'),
        action: 'getSupport'
      }
    ];
  }.property(),

  actions: {
    setAsHome() {
      this.sendAction('setAsHome');
    },

    leaveSpace() {
      this.sendAction('openSettingsModal', 'leave', this.get('space'));
    },

    renameSpace() {
      this.sendAction('openSettingsModal', 'rename', this.get('space'));
    },

    removeSpace() {
      let i18n = this.get('i18n');
      this.get('messageBox').open({
        title: i18n.t('common.featureNotSupportedShort'),
        type: 'warning',
        allowClose: false,
        message: i18n.t('common.featureNotSupportedLong')
      });

      // TODO: remove function currently disabled
      // this.sendAction('openSettingsModal', 'remove', this.get('space'));
    },

    inviteGroup() {
      this.get('commonModals').openModal('token', {
        type: 'groupJoinSpace',
        funArgs: [this.get('space.id')],
      });
    },

    inviteUser() {
      this.get('commonModals').openModal('token', {
        funArgs: [this.get('space.id')],
        type: 'userJoinSpace'
      });
    },

    getSupport() {
      this.get('commonModals').openModal('token', {
        funArgs: [this.get('space.id')],
        type: 'providerSupport'
      });
    }
  }
});
