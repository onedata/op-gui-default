/**
 * A debugging/testing util for invoking callback on component init.
 * 
 * It can be used eg. in integration tests for testing yielded values or
 * if debugging templates rendering to find out how many times the component
 * is initialized.
 * 
 * Example:
 * - template
 * `
 * {{#some-component as |data|}}
 *   {{test-callback callback=(action "hello") x=data.foo}}
 * {{/some-component}}
 * `
 * - template action
 * `
 * hello(testCallbackComponent) {
 *   console.log(testCallbackComponent.get('x'));
 * }
 * `
 * - some-parent component template
 * `
 * yield (hash foo="bar")
 * `
 * 
 * See component integration tests for working examples.
 * 
 * @module components/test-callback
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// TODO: do not use this in passed callback - instead use "self" which will mean a component
 
import Ember from 'ember';

const {
  Component,
} = Ember;

export default Component.extend({  
  classNames: ['test-callback'],
  
  /**
   * @virtual
   * A function that will be invoked with one argument - instance of `test-callback`
   * on component init
   * @type {Function}
   */
  callback: undefined,
  
  init() {
    this._super(...arguments);
    this.get('callback')(this);
  },
});
