import Ember from 'ember';

/**
 * Icon in main sidebar (main navigation).
 *
 * Send actions:
 * - activateItem(itemName)
 * @module components/main-menu-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'li',
  name: null,
  classNameBindings: ['isActive:active'],

  mainMenu: Ember.inject.service(),

  isActive: function() {
    return this.get('mainMenu.currentItem') === this.get('name');
  }.property('name', 'mainMenu.currentItem'),

  actions: {
    activateItem() {
      if (this.get('link')) {
        window.location = this.get('link');
      } else {
        this.sendAction('goToItem', this.get('name'));
      }
    }
  }
});
