
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
   * @virtual optional
   * If true, an external link icon will be shown
   * @type {boolean}
   */
  isExternal: false,
  
  /**
   * @virtual
   * Link route target
   * @type {string}
   */
  link: undefined,

  /**
   * @virtual
   * Id of item
   * @type {string}
   */
  name: undefined,
  
  /**
   * @virtual 
   * Text displayed in menu item
   * @type {string}
   */
  title: undefined,
  
  /**
   * @virtual
   * Name of oneicon to use
   * @type {string}
   */
  icon: undefined,

  isActive: computed('name', 'mainMenu.currentItem', function() {
    return this.get('mainMenu.currentItem') === this.get('name');
  }),

  anchorId: computed('name', function () {
    return `main-${this.get('name')}`;
  }),
});
