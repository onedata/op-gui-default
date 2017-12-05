/**
 * An entry for single space in transfers-menu.
 * It's just a button for opening transfers view for a space
 *
 * @module components/transfers-menu-item
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  inject: { service },
  computed,
} = Ember;

export default Component.extend({
  tagName: 'li',
  classNames: ['first-level', 'hover-parent'],
  classNameBindings: ['isActive:active'],

  secondaryMenu: service(),

  /**
   * @virtual
   */
  space: undefined,
  
  isActive: computed('secondaryMenu.activeItem.id', 'space.id', function() {
    return this.get('secondaryMenu.activeItem.id') === this.get('space.id');
  }),

  actions: {
    /**
     * Should show transfers view for space
     */
    activate() {
      this.set('secondaryMenu.activeItem', this.get('space'));
    },
  }
});
