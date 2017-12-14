/**
 * Renders "three dots" button with popover menu. Creates menu items using
 * menuActions array, which each element is an object in format:
 * {
 *  title {string},
 *  action {callback},
 *  icon {string}
 * }
 * 
 * @module components/dots-menu
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ClickOutside from 'ember-click-outside/mixins/click-outside';
import bindFloater from 'ember-cli-onedata-common/utils/bind-floater';
import layout from '../templates/components/dots-menu';

const {
  Component,
  computed,
  run,
} = Ember;

export default Component.extend(ClickOutside, {
  layout,
  attributeBindings: ['style'],
  classNames: ['dots-menu'],
  classNameBindings: ['isOpened:open'],

  /**
   * Array of menu items.
   * @type {Array<Object>}
   * @virtual
   */
  menuActions: [],

  /**
   * Selector for the parent element, which scroll event should be listened to
   * perform popover autoclose
   * @type {string}
   */
  scrollableParentSelector: undefined,

  /**
   * X position. Possible values: left, right.
   * @type {string}
   */
  posX: 'left',

  /**
   * If true, popover menu is opened.
   * @type {boolean}
   */
  isOpened: false,

  /**
   * Callback, that recalculates popover position
   * @type {function}
   */
  refreshPosition: () => {},
  
  /**
   * Selector for the element, which is a target point where popover
   * should be rendered
   * @type {Emebr.ComputedProperty<string>}
   */
  bindSelector: computed('elementId', function() {
    return `#${this.get('elementId')} .dots-trigger`;
  }),

  /**
   * Specific css classes for x postion
   * @type {Emebr.ComputedProperty<string>}
   */
  posXClasses: computed('posX', function () {
    switch (this.get('posX')) {
      case 'right':
        return 'dropdown-menu-right';
      case 'left':
        return 'dropdown-menu-left';
    }
  }),

  /**
   * Parent element scroll event handler. It closes popover.
   * @type {Ember.ComputedProprty<function>}
   */
  scrollEventHandler: computed(function () {
    return () => {
      this.set('isOpened', false);
    };
  }),

  didInsertElement() {
    const {
      bindSelector,
      posX,
      scrollableParentSelector,
      scrollEventHandler,
    } = this.getProperties(
      'bindSelector',
      'posX',
      'scrollableParentSelector',
      'scrollEventHandler'
    );
    if (bindSelector) {
      const bindElement = $(bindSelector);
      this.set('refreshPosition', bindFloater(this.$('.dropdown-menu'), bindElement, {
        posX,
        posY: 'top',
        offsetY: -5,
        offsetX: posX === 'right' ? bindElement.outerWidth() : 0,
      }));
    }
    if (scrollableParentSelector) {
      $(scrollableParentSelector).scroll(scrollEventHandler);
    }
    $(window).resize(scrollEventHandler);
    run.next(this, this.addClickOutsideListener);
  },
  
  willDestroyElement() {
    const {
      scrollableParentSelector,
      scrollEventHandler,
    } = this.getProperties('scrollableParentSelector', 'scrollEventHandler');
    this.removeClickOutsideListener();
    if (scrollableParentSelector) {
      $(scrollableParentSelector).unbind('scroll', scrollEventHandler);
    }
    $(window).unbind('resize', scrollEventHandler);
  },
  
  clickOutside() {
    this.set('isOpened', false);
  },

  actions: {
    toggleVisibility() {
      this.get('refreshPosition')();
      this.toggleProperty('isOpened');
    },
    runAction(index) {
      this.get('menuActions')[index].action();
      this.set('isOpened', false);
    },
  }
});
