/**
 * A submenu for spaces-menu, displaying options page for Space permissions:
 * users, groups or providers
 *
 * Send actions:
 * - showUsersConfig(space)
 * - showGroupsConfig(space)
 *
 * @module components/spaces-submenu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'ul',
  classNames: ['submenu'],

  spacesMenu: Ember.inject.service(),

  isSpaceActive: function() {
    return this.get('space.id') === this.get('spacesMenu.activeSpace.id');
  }.property('space.id', 'spacesMenu.activeSpace.id'),

  // NOTE: readonly property, if want to modify, set spacesMenu.activeOption
  activeOption: function() {
    return this.get('isSpaceActive') ? this.get('spacesMenu.activeOption') : null;
  }.property('isSpaceActive', 'spacesMenu.activeOption'),

  space: null,

  sidebarEntryId: function() {
    return this.get('space.sidebarEntryId');
  }.property('space', 'space.sidebarEntryId'),

  activeOptionDidChange: function() {
    if (this.get('isSpaceActive') && this.get('activeOption')) {
      this.sendAction('openSubmenuEntry', this.get('activeOption'));
    }
  }.observes('activeOption'),

  actions: {
    setOption(optionName) {
      console.debug(`set active option to ${optionName}`);
      this.set('activeOption', optionName);
    },
  }

});
