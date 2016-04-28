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
    Example usage:
    ```
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
    ```
    @param {RSVP.Promise} promise A promise to bind loading set/unset
    @returns {RSVP.Promise} Passed promise
  */
  promiseLoading(promise) {
    this.set('isLoading', true);
    promise.finally(() => this.set('isLoading', false));
    return promise;
  }
});
