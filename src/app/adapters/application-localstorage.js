import Ember from 'ember';

/**
 * Extends LocalstorageAdapter with mocks of Onedata adapter methods.
 * @module adapters/base/application-in-memory
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

 import LSA from './base/localstorage';

 export default LSA.extend({
  /** Called automatically on adapter init. */
  init() {},

  /** Initializes the WebSocket */
  initWebSocket(onOpen, onError) {
    console.debug(`Fake websocket init`);
    if (onOpen) {
      this.set('onOpenCallback', onOpen);
    }
    if (onError) {
      this.set('onErrorCallback', onError);
    }
    if (this.get('initialized') === false) {
      this.set('initialized', true);
    }

    if (onOpen) {
       onOpen();
    }
  },

  RPC(type, operation, data) {
    console.debug(`Fake RPC:
      type: ${type}
      operation: ${operation}
      data: ${JSON.stringify(data)}`);

    switch (type) {
      case 'session':
        return new Ember.RSVP.Promise((resolve) => {
          resolve({
            "sessionValid": true,
            "sessionDetails": {
              "userName":"Fake User",
              "manageProvidersURL":"https://veillsdev.com/#/onezone",
              "connectionRef":"testconnection"
            }
          });
        });
      default:
        return new Ember.RSVP.Promise((resolve) => {
          resolve('hello', 'world');
        });
    }
    // TODO: mock type == session
    // TODO: mock type == public
    // TODO: mock type == private
  }
});
