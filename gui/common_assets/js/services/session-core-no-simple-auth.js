// jshint esnext: true

/**
 * Provides a session abstraction. The session validity
 * is resolved via WebSocket.
 * @module services/session
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// This file should be linked to app/services/session.js

import Ember from 'ember';

export default Ember.Service.extend({
  server: Ember.inject.service('server'),

  sessionInitResolve: null,
  sessionInitReject: null,

  // This flag indicates if the client has active session. Null when
  // the session validity hasn't been resolved yet.
  sessionValid: null,
  // Generic session details (object) returned from the server, containing
  // user name etc.
  sessionDetails: null,

  /**
   * Returns a function that shout be bound to websocket onerror event.
   */
  onWebSocketError: function() {
    return (/*event*/) => {
      // Reject session restoration if WebSocket connection
      // could not be established
      const initRejectFunction = this.get('sessionInitReject');
      if (initRejectFunction) {
        console.debug("SESSION INIT REJECTED");
        initRejectFunction();
      }
      const restoreRejectFunction = this.get('sessionRestoreReject');
      if (restoreRejectFunction) {
        console.debug("SESSION RESTORE REJECTED");
        restoreRejectFunction();
      }
      this.setProperties({
        sessionInitResolve: null,
        sessionInitReject: null
      });
    };
  }.property(),

  /**
   * @abstract
   * Should return a function that shout be bound to websocket onclose event.
   */
  onWebSocketClose: null,


    /**
     * Returns a function that shout be bound to websocket onopen event.
     */
    onWebSocketOpen: function() {
      // Ask the server for session details when the WebSocket connection
      // is established
      return (/*event*/) => {
        this.set('websocketWasOpened', true);
        this.resolveSession();
      };
    }.property(),

    /**
     * Returns a function that shout be bound to websocket onerror event.
     */
    onWebSocketError: function() {
      return (/*event*/) => {
        // Reject session restoration if WebSocket connection
        // could not be established
        const initRejectFunction = this.get('sessionInitReject');
        if (initRejectFunction) {
          console.debug("SESSION INIT REJECTED");
          initRejectFunction();
        }
        const restoreRejectFunction = this.get('sessionRestoreReject');
        if (restoreRejectFunction) {
          console.debug("SESSION RESTORE REJECTED");
          restoreRejectFunction();
        }
        this.setProperties({
          sessionInitResolve: null,
          sessionInitReject: null,
          sessionRestoreResolve: null,
          sessionRestoreReject: null
        });
      };
    }.property(),

  /** Returns a promise that will be resolved when the client has resolved
   * its session using WebSocket.
   * NOTE: This requires server service and WebSocket adapter.
   * If this is called, session data from WebSocket will resolve session
   * restoration rather than run authenticate. */
  initSession: function () {
    // Initialize the WebSocket and, when it is done, resolve simple-auth
    // session.
    this.get('server').initWebSocket(
      this.get('onWebSocketOpen'),
      this.get('onWebSocketError'),
      this.get('onWebSocketClose')
    );

    return new Ember.RSVP.Promise((resolve, reject) => {
      // This promise will be resolved when WS connection is established
      // and session details are sent via WS.
      this.setProperties({
        sessionInitResolve: resolve,
        sessionInitReject: reject
      });
    });
  },

  /** Performs an RPC call and registers a promise that will resolve
   * client session when WebSocket is established and it has send session
   * details. */
  resolveSession: function () {
    console.debug('session.resolveSession');
    // Request session data
    this.get('server').sessionRPC().then((data) => {
      console.debug("RESOLVE SESSION REQ");
      console.debug('data: ' + JSON.stringify(data));
      this.set('sessionValid', data.sessionValid);
      if (this.get('sessionValid') === true) {
        this.set('sessionDetails', data.sessionDetails);
      }
      let resolveFunction = this.get('sessionInitResolve');
      resolveFunction();
      this.set('sessionInitResolve', null);
      this.set('sessionInitReject', null);
    });
  }
});
