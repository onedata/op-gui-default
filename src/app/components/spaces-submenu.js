/**
 * A submenu for spaces-menu, displaying options page for Space permissions:
 * users, groups or providers (using spaces-submenu-item)
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

  secondaryMenu: Ember.inject.service(),
  activeSpace: Ember.computed.alias('secondaryMenu.activeSpace'),

  isSpaceActive: function() {
    return this.get('space.id') === this.get('activeSpace.id');
  }.property('space.id', 'activeSpace.id'),

  // NOTE: readonly property, if want to modify, set secondaryMenu.activeOption
  activeOption: function() {
    return this.get('isSpaceActive') ? this.get('secondaryMenu.activeOption') : null;
  }.property('isSpaceActive', 'secondaryMenu.activeOption'),

  space: null,

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
