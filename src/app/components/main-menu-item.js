
/**
 * Icon in main sidebar (main navigation).
 *
 * Send actions:
 * - activateItem(itemName)
 * @module components/main-menu-item
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  computed,
} = Ember;

export default Ember.Component.extend({
  tagName: 'li',
  classNameBindings: ['isActive:active'],

  mainMenu: Ember.inject.service(),

  /**
   * @virtual
   * @type {string}
   */
  link: undefined,

  /**
   * @virtual
   * @type {string}
   */
  name: undefined,

  isActive: computed('name', 'mainMenu.currentItem', function() {
    return this.get('mainMenu.currentItem') === this.get('name');
  }),

  anchorId: computed('name', function () {
    return `main-${this.get('name')}`;
  }),
});
