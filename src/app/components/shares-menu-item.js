import Ember from 'ember';

/**
 * An entry for single share in shares-menu.
 * It contains share options dropdown and submenu with permissions.
 * Share-manipulation actions are delegated to shares-menu, because most actions
 * needs to have access to full shares list.
 *
 * @module components/shares-menu-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'li',
  classNames: ['first-level', 'hover-parent'],
  classNameBindings: ['isExpanded:active'],

  secondaryMenu: Ember.inject.service(),

  isExpanded: function() {
    return this.get('secondaryMenu.activeItem.id') === this.get('share.id');
  }.property('secondaryMenu.activeItem.id'),

  actions: {
    /** Delegate to goToShare action, should show submenu to configure Share */
    expand() {
      this.set('secondaryMenu.activeItem', this.get('share'));
    },

    openSubmenuEntry(name) {
      console.debug(`shares-menu-item: openSubmenuEntry(${name})`);
      this.sendAction('openSubmenuEntry', this.get('share'), name);
    },

    /**
     * Pass the action up, modals are placed in shares-menu
     */
    openSettingsModal(modalName, share) {
      this.sendAction('openSettingsModal', modalName, share);
    },
  }
});
