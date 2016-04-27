import Ember from 'ember';

/**
 * An entry for single space in spaces-menu.
 * It contains space options dropdown and submenu with permissions.
 * Space-manipulation actions are delegated to spaces-menu, because most actions
 * needs to have access to full spaces list.
 *
 * @module components/spaces-menu-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'li',
  classNames: ['first-level', 'hover-parent'],
  classNameBindings: ['isExpanded:active'],
  setElementId: Ember.on('init', function() {
    this.set('elementId', this.get('space.sidebarEntryId'));
  }),

  spacesMenu: Ember.inject.service(),

  isExpanded: function() {
    return this.get('spacesMenu.activeSpace.id') === this.get('space.id');
  }.property('spacesMenu.activeSpace.id'),

  actions: {
    /** Delegate to goToSpace action, should show submenu to configure Space */
    expand() {
      this.set('spacesMenu.activeSpace', this.get('space'));
    },

    openSubmenuEntry(name) {
      console.debug(`spaces-menu-item: openSubmenuEntry(${name})`);
      this.sendAction('openSubmenuEntry', this.get('space'), name);
    },

    openSettingsModal(modalName, space) {
      this.sendAction('openSettingsModal', modalName, space);
    },

    setAsHome() {
      this.sendAction('setAsHome', this.get('space'));
    }
  }
});
