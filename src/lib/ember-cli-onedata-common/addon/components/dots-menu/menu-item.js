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
  
  click() {
    if (!this.get('disabled')) {
      this.get('action')();
    }
  },
});
