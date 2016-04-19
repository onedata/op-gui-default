import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

export default Ember.Component.extend({
  commonModals: Ember.inject.service(),

  classNames: ['item-element', 'item-icon'],

  /** Items in "space settings" dropright menu */
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
      {
        icon: 'remove',
        label: i18n.t('components.spacesMenu.drop.remove'),
        action: 'removeSpace'
      },
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

  // TODO: deregister event from sidebar?
  didInsertElement() {
    let sidebar = $('.secondary-sidebar');
    let drop = this.$().find('.dropdown-menu');
    let updater = bindFloater(drop, null, {
      offsetX: 8
    });
    sidebar.on('scroll', updater);
    drop.on('mouseover', updater);
    // TODO: this hack is probably not needed anymore
    drop.closest('.settings-dropdown').on('click', function() {
      window.setTimeout(() => {
        updater();
      }, 50);
    });
  },

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
      this.sendAction('openSettingsModal', 'remove', this.get('space'));
    },

    inviteGroup() {
      this.get('commonModals').openModal('token-group', {
        space: this.get('space')
      });
    },

    inviteUser() {
      this.get('commonModals').openModal('token-user', {
        space: this.get('space')
      });
    },

    getSupport() {
      this.get('commonModals').openModal('token-support', {
        space: this.get('space')
      });
    }
  }
});
