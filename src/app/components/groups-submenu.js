/**
 * A submenu for groups-menu, displaying options page for Group permissions:
 * users, groups or providers (using groups-submenu-item)
 *
 * @module components/groups-submenu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'ul',
  classNames: ['submenu'],

  groupsMenu: Ember.inject.service(),

  isGroupActive: function() {
    return this.get('group.id') === this.get('groupsMenu.activeGroup.id');
  }.property('group.id', 'groupsMenu.activeGroup.id'),

  // NOTE: readonly property, if want to modify, set groupsMenu.activeOption
  activeOption: function() {
    return this.get('isGroupActive') ? this.get('groupsMenu.activeOption') : null;
  }.property('isGroupActive', 'groupsMenu.activeOption'),

  group: null,

  activeOptionDidChange: function() {
    if (this.get('isGroupActive') && this.get('activeOption')) {
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
