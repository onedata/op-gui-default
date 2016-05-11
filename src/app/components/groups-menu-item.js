import Ember from 'ember';

/**
 * An entry for single group in groups-menu.
 * It contains group options dropdown and submenu with permissions.
 * Group-manipulation actions are delegated to groups-menu, because most actions
 * needs to have access to full groups list.
 *
 * @module components/groups-menu-item
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
    return this.get('secondaryMenu.activeItem.id') === this.get('group.id');
  }.property('secondaryMenu.activeItem.id'),

  actions: {
    /** Delegate to goToGroup action, should show submenu to configure Group */
    expand() {
      this.set('secondaryMenu.activeItem', this.get('group'));
    },

    openSubmenuEntry(name) {
      console.debug(`groups-menu-item: openSubmenuEntry(${name})`);
      this.sendAction('openSubmenuEntry', this.get('group'), name);
    },

    openSettingsModal(modalName, group) {
      this.sendAction('openSettingsModal', modalName, group);
    },
  }
});
