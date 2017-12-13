/**
 * 
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
  get,
  run,
} = Ember;

export default Component.extend(ClickOutside, {
  layout,
  attributeBindings: ['style'],
  classNames: ['dots-menu'],
  classNameBindings: ['isOpened:open'],

  posX: 'left',

  isOpened: false,

  menuActions: [],

  refreshPosition: () => {},
  
  bindSelector: computed('elementId', function() {
    return `#${this.get('elementId')} .dots-trigger`;
  }),

  posXClasses: computed('posX', function () {
    switch (this.get('posX')) {
      case 'right':
        return 'dropdown-menu-right';
      case 'left':
        return 'dropdown-menu-left';
    }
  }),

  didInsertElement() {
    const {
      bindSelector,
      posX,
     } = this.getProperties('bindSelector', 'posX');
    if (bindSelector) {
      const bindElement = $(bindSelector);
      this.set('refreshPosition', bindFloater(this.$('.dropdown-menu'), bindElement, {
        posX,
        posY: 'top',
        offsetY: -5,
        offsetX: posX === 'right' ? bindElement.outerWidth() : 0,
      }));
    }
    run.next(this, this.addClickOutsideListener);
  },
  
  willDestroyElement() {
    this.removeClickOutsideListener();
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
