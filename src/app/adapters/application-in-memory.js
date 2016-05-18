import InMemoryAdapter from 'ember-data-in-memory-adapter';

/**
 * Extends EmberDataInMemoryAdapter with mocks of Onedata adapter methods.
 * @module adapters/base/application-in-memory
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default InMemoryAdapter.extend({
  /** Called automatically on adapter init. */
  init() {
    this.initWebSocket();
  },

  /** Initializes the WebSocket */
  initWebSocket(/*onOpen, onError*/) {
    console.debug(`Fake websocket init`);
  },
});
