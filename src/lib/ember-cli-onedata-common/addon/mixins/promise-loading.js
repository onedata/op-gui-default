import Ember from 'ember';

/**
 * Mixin to set loading state (isLoading property) on promise start, and unset
 * loading on resolve/reject.
 * The inheriting class should use boolean isLoading property to set "loading"
 * styles and elements (e.g. spinners).
 * @module mixins/promise-loading
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  /**
    By default, sets 'isLoading' property to true, and registers setting
    'isLoading' to false on promise resolve/reject.
    However, custom callback can be set to override this behaviour.
    @example
    this.promiseLoading(
      this.get('onezoneServer').getConnectAccountEndpoint(providerName)
    ).then(
      (url) => {
        window.location = url;
      },
      (error) => {
        console.error(error.message);
      }
    });

    @param {RSVP.Promise} promise A promise to bind loading set/unset
    @param {promiseLoadingBeforeCallback} [callbackBefore] A callback to invoke
      in this method
    @param {promiseLoadingAfterCallback} [callbackAfter] A callback to invoke
      on promise reolve and reject
    @returns {RSVP.Promise} Passed promise
  */
  promiseLoading(promise, callbackBefore, callbackAfter) {
    if (!callbackBefore) {
      callbackBefore = () => this.set('isLoading', true);
    }

    if (!callbackAfter) {
      callbackAfter = () => this.set('isLoading', false);
    }

    callbackBefore();
    promise.finally(callbackAfter);
    return promise;
  }
});

/**
 * @callback promiseLoadingBeforeCallback
 */

 /**
  * @callback promiseLoadingAfterCallback
  */
