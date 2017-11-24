/**
 * Item header class of the collapsible list. For example of use case see 
 * components/one-collapsible-list.js.
 * 
 * If toolbarWhenOpened == true then .btn-toolbar elements will be 
 * visible only if the list item is expanded.
 * isCollapsible == false disables collapse functionality.
 * 
 * Yields:
 * - toggleAction - action, that toggles list item visibility
 * 
 * Module imported from onedata-gui-common.
 *
 * @module components/one-collapsible-list-item-header.js
 * @author Michał Borzęcki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  observer,
} = Ember;

export default Ember.Component.extend({
  tagName: 'div',
  classNames: ['one-collapsible-list-item-header', 'row', 'list-header-row', 'truncate'],
  classNameBindings: [
    'isOpened:opened',
    'isCollapsible:collapsible',
    'toolbarWhenOpened:toolbar-when-opened',
    'disableToggleIcon:disable-toggle-icon',
    '_isItemFixed:header-fixed'
  ],

  /**
   * If true, do not render toggle icon even if header if isCollapsible
   * @type {boolean}
   */
  disableToggleIcon: false,

  /**
   * If true, item can be collapsed
   * @type {boolean}
   */
  isCollapsible: true,

  toggle: () => {},

  /**
   * A selector for elements, which click actions should be ignored by item 
   * toggle event handler
   * @type {string}
   */
  _clickDisabledElementsSelector: '.btn-toolbar *, .webui-popover *, .item-checkbox, .item-checkbox *',

  _clickHandlerObserver: observer('_isItemFixed', 'isCollapsible', function () {
    let {
      _isItemFixed,
      isCollapsible,
      click,
    } = this.getProperties(
      '_isItemFixed',
      'isCollapsible',
      'click'
    );
    if (click === this._clickHandler || !click) {
      this.set(
        'click', !_isItemFixed && isCollapsible ? this._clickHandler : undefined
      );
    }
  }),

  init() {
    this._super(...arguments);
    this._clickHandlerObserver();
  },

  /**
   * Click handler for click() component method
   * @param {Event} event 
   */
  _clickHandler(event) {
    let selector = this.get('_clickDisabledElementsSelector');
    if ((event.target.matches && event.target.matches(selector)) ||
      (event.target.msMatchesSelector && event.target.msMatchesSelector(selector))) {
      event.stopPropagation();
    } else {
      this.send('toggle');
    }
  },

  actions: {
    /**
     * Toggles collapse state of the collapsible item
     * @param {boolean} opened should item be opened or collapsed?
     */
    toggle(opened) {
      if (!this.get('_isItemFixed') && this.get('isCollapsible')) {
        this.get('toggle')(opened);
      }
    }
  }
});
