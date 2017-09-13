/**
 * Provides a session abstraction using ember-simple-auth. The session validity
 * is resolved via WebSocket.
 * @module services/session
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// This file should be linked to app/services/session.js

import Ember from 'ember';
import SessionService from 'ember-simple-auth/services/session';

export default SessionService.extend({
  server: Ember.inject.service(),
  store: Ember.inject.service(),

  /**
   * @type {function}
   * A resolve function bound in initSession or tryToRestoreSession.
   * It will resolve the promise returned in initSession or tryToRestoreSession.
   *
   * - Value of this properties are set in initSession or tryToRestoreSession
   * - Value is nulled in resolveSession and onWebsocketError
   */
  sessionInitResolve: null,

  /**
   * @type {function}
   * A reject function bound in initSession or tryToRestoreSession.
   * It will reject the promise returned in initSession or tryToRestoreSession.
   *
   * - Value of this properties are set in initSession or tryToRestoreSession
   * - Value is nulled in resolveSession and onWebsocketError
   */
  sessionInitReject: null,
  sessionRestoreResolve: null,
  sessionRestoreReject: null,

  /**
   * This flag indicates if the client has active session. Null when
   * the session validity hasn't been resolved yet.
   */
  sessionValid: null,

  /**
   * Generic session details (object) returned from the server.
   * @type {Object}
   * @property {boolean} firstLogin
   * @property {string} userId - id of User record
   */
  sessionDetails: null,

  /**
   * A User record.
   * @type {User}
   */
  user: null,

  websocketWasOpened: false,

  /**
   * Returns a function that shout be bound to websocket onopen event.
   */
  onWebSocketOpen: function() {
    // Ask the server for session details when the WebSocket connection
    // is established
    return (/*event*/) => {
      this.resolveSession();
      this.setProperties({
        websocketWasOpened: true,
        websocketOpen: true
      });
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

  /**
   * @abstract
   * Should return a function that shout be bound to websocket onclose event.
   */
  onWebSocketClose: null,

  /** Returns a promise that will be resolved when the client has resolved
   * its session using WebSocket.
   * NOTE: This requires server service and WebSocket adapter.
   * If this is called, session data from WebSocket will resolve session
   * restoration rather than run authenticate. */
  initSession: function () {
    // bind session service websocket event handlers
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

  /** If this is called, session data from WebSocket will resolve session
   * restoration rather than run authenticate. */
  tryToRestoreSession: function () {
    return new Ember.RSVP.Promise((resolve, reject) => {
      console.debug('tryToRestoreSession, sessionValid = ', this.get('sessionValid'));
      if (this.get('sessionValid') === true) {
        resolve();
      } else {
        // This promise will be resolved when WS connection is established
        // and session details are sent via WS.
        this.setProperties({
          sessionRestoreResolve: resolve,
          sessionRestoreReject: reject
        });
      }
    });
  },

  /** Performs an RPC call and registers a promise that will resolve
   * client session when WebSocket is established and it has send session
   * details. */
  resolveSession() {
    console.debug('session.resolveSession');
    // Request session data
    // TODO: no reject handling
    this.get('server').sessionRPC().then((data) => {
      console.debug("RESOLVE SESSION REQ");
      console.debug('data: ' + JSON.stringify(data));
      let isSessionValid = (data.sessionValid === true);
      this.set('sessionValid', isSessionValid);
      if (isSessionValid) {
        this.onResolveSessionValid(data);
      } else {
        this.onResolveSessionInvalid();
      }
      const resolveFunction = this.get('sessionInitResolve');
      // the resoleFunction can be undefined/null only if we (re)open WebSocket
      // only, without reinitializing session
      if (resolveFunction) {
        resolveFunction();
      }

      this.setProperties({
        sessionInitResolve: null,
        sessionInitReject: null,
        sessionRestoreResolve: null,
        sessionRestoreReject: null
      });
    });
  },

  /**
   * @private
   */
  onResolveSessionValid(data) {
    let sessionRestoreResolveFun = this.get('sessionRestoreResolve');
    Ember.assert(
      'session data should contain sessionDetails object',
      data.sessionDetails != null
    );
    Ember.assert(
      'sessionDetails should include userId property',
      data.sessionDetails.userId
    );
    let sessionUserPromise = this.getUser(data.sessionDetails.userId);
    sessionUserPromise.then(user => {
      this.setProperties({
        sessionDetails: data.sessionDetails,
        user: user
      });
      if (sessionRestoreResolveFun) {
        console.debug("SESSION VALID, RESTORED");
        sessionRestoreResolveFun();
      } else {
        console.debug("SESSION VALID, AUTHENTICATED");
        this.get('session').authenticate('authenticator:basic');
      }
    });
    sessionUserPromise.catch(() => {
      console.debug("SESSION: USER CANNOT BE FETCHED (findRecord rejected)");
      this.onResolveSessionInvalid();
    });
  },

  /**
   * @private
   */
  onResolveSessionInvalid() {
    console.debug("SESSION INVALID");
    const sessionRestoreRejectFun = this.get('sessionRestoreReject');
    if (sessionRestoreRejectFun) {
      console.debug("RESTORE REJECTED");
      sessionRestoreRejectFun();
    }
  },

  /**
   * @private
   */
  getUser(userId) {
    return this.get('store').findRecord('user', userId);
  }
});
