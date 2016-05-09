import InMemoryAdapter from 'ember-data-in-memory-adapter';

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
