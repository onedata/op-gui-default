/**
 * A menu item in dots-menu which renders an icon and a title.
 * An action can be bind on click (use `action` property)
 *
 * @module components/dots-menu/menu-item
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import layout from '../../templates/components/dots-menu/menu-item';

const {
  Component,
} = Ember;

export default Component.extend({
  layout,
  
  tagName: 'li',
  classNames: ['dots-menu-item'],
  classNameBindings: ['disabled:disabled'],
  
  /**
   * @virtual
   * @type {string}
   */
  icon: undefined,
  
  /**
   * @virtual
   * @type {string}
   */
  title: undefined,
  
  /**
   * @public
   * If set to true - the button looks like disabled and is not clickable
   * @type {boolean}
   */
  disabled: false,
  
  /**
   * @virtual
   * @type {function}
   */
  action: () => {},
  
  click(event) {
    if (!this.get('disabled')) {
      this.get('action')();
    }
    event.stopPropagation();
  },
});
