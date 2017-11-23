// FIXME: jsdoc

import Ember from 'ember';

const {
  Component,
} = Ember;

export default Component.extend({  
  classNames: ['test-callback'],
  
  /**
   * @virtual
   */
  callback: undefined,
  
  init() {
    this._super(...arguments);
    this.get('callback').bind(this)();
  },
});
