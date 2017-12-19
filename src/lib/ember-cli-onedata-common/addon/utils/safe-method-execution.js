/**
 * NOTE: backported from `onedata-gui-common`
 * 
 * Invoke method on Ember.Object with checking destroy flags
 *
 * @module utils/safe-method-execution
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 * 
 * @param {Ember.Object} obj 
 * @param {string|function} method
 * @returns {any} value returned by method or undefined on destroying error
 */
export default function safeMethodExecution(obj, method, ...params) {
  const methodType = (typeof method);
  if (!obj.isDestroyed && !obj.isDestroying) {
    if (methodType === 'function') {
      return method.bind(obj)(...params);
    } else if (methodType === 'string') {
      return obj[method](...params);
    } else {
      throw new Error(
        'util:safe-method-execution: passed method is not string nor function');
    }
  } else {
    const methodString = (methodType === 'function' && method.name || method.toString());
    console.warn(
      `util:safe-method-execution: Cannot execute "${methodString}" on ` +
      `Ember.Object because it is destroyed`
    );
  }
}
