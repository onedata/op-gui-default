/**
 * Provides communication with Server.
 * @module services/server
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// This file should be linked to app/services/server.js

import Ember from 'ember';

const {
  Service,
  inject: { service },
  computed,
} = Ember;

export default Service.extend({
  store: service('store'),
  websocketConnection: service(),

  adapter: computed(function adapter() {
    return this.get('store').adapterFor('application');
  }),

  /**
   * Forces the WebSocket adapter to initialize a WebSocket connection.
   * Can register onOpen, onError, onClose callbacks that will be called after
   * the connection is established, refused or closed.
   *
   * onOpen, onError and onClose arguments are event handlers for WebSocket events
   *
   * See WebSocket events on: https://developer.mozilla.org/en-US/docs/Web/Events
   */
  initWebSocket(onOpen, onError, onClose, isPublic) {
    return this.get('websocketConnection')
      .initWebSocket(onOpen, onError, onClose, isPublic);
  },

  clearWebsocket() {
    return this.get('websocketConnection').clearWebsocket();
  },

  closeWebsocket() {
    return this.get('websocketConnection').closeWebsocket();
  },

  /**
   * Sends a RPC call via WebSocket asking for session data, i.e. if the session
   * is valid and session details such as user name.
   * Returns a promise that will be called with received data.
   */
  sessionRPC: function () {
    return this.get('adapter').RPC('session');
  },

  /**
   * Sends an RPC call to the server for a publicly available resource.
   * Returns a promise that will be called with received data.
   */
  publicRPC: function (operation, data) {
    return this.get('adapter').RPC('public', operation, data);
  },

  /**
   * Sends an RPC call to the server for a resource that is restricted to
   * logged in clients.
   * Returns a promise that will be called with received data.
   */
  privateRPC: function (operation, data) {
    return this.get('adapter').RPC('private', operation, data);
  }
});
