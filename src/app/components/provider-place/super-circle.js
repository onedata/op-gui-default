// FIXME: jsdoc

import Ember from 'ember';

const {
  Component,
} = Ember;

export default Component.extend({
  classNames: ['super-circle'],
  classNameBindings: ['isSource:source', 'isDestination:destination'],
  
  /**
   * @type {boolean}
   */
  isSource: false,
  
  /**
   * @type {boolean}
   */
  isDestination: false,
});
