// jshint esnext: true

/**
 * Provides a session abstraction using ember-simple-auth. The session validity
 * is resolved via WebSocket.
 * @module services/server
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

  /** Returns a promise that will be resolved when the client has resolved
   * its session using WebSocket.
   * NOTE: This requires server service and WebSocket adapter.
   * If this is called, session data from WebSocket will resolve session
   * restoration rather than run authenticate. */
  initSession: function () {
    let session = this;
    // Initialize the WebSocket and, when it is done, resolve simple-auth
    // session.
    let onOpen = () => {
      // Ask the server for session details when the WebSocket connection
      // is established
      session.resolveSession();
    };
    let onError = () => {
      // Reject session restoration if WebSocket connection
      // could not be established
      let initRejectFunction = this.get('sessionInitReject');
      if (initRejectFunction) {
        console.log("SESSION INIT REJECTED");
        initRejectFunction();
      }
      let restoreRejectFunction = this.get('sessionRestoreReject');
      if (restoreRejectFunction) {
        console.log("SESSION RESTORE REJECTED");
        restoreRejectFunction();
      }
      this.set('sessionInitResolve', null);
      this.set('sessionInitReject', null);
    };
    this.get('server').initWebSocket(onOpen, onError);
    return new Ember.RSVP.Promise((resolve, reject) => {
      // This promise will be resolved when WS connection is established
      // and session details are sent via WS.
      this.set('sessionInitResolve', resolve);
      this.set('sessionInitReject', reject);
    });
  },

  /** Performs an RPC call and registers a promise that will resolve
   * client session when WebSocket is established and it has send session
   * details. */
  resolveSession: function () {
    console.log('session.resolveSession');
    // Request session data
    this.get('server').sessionRPC().then((data) => {
      console.log("RESOLVE SESSION REQ");
      console.log('data: ' + JSON.stringify(data));
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
