/**
 * Hacky session service (please do not ask)
 * @module services/session
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// This file should be linked to app/services/session.js

import Ember from 'ember';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import onezoneUrl from 'op-worker-gui/utils/onezone-url';

const {
  computed,
  Service,
  inject: { service },
  RSVP: { Promise },
  run: { next },
} = Ember;

export default Service.extend({
  server: service(),
  store: service(),

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
  onWebSocketOpen: computed(function () {
    // Ask the server for session details when the WebSocket connection
    // is established
    return ( /*event*/ ) => {
      return this.resolveSession().finally(() => {
        this.setProperties({
          websocketWasOpened: true,
          websocketOpen: true
        });
      });
    };
  }),

  /**
   * Returns a function that shout be bound to websocket onerror event.
   */
  onWebSocketError: function () {
    return ( /*event*/ ) => {
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
      return new Promise(() => {
        next(() => {
          if (!window.onedataIsReloadingApp) {
            sessionStorage.setItem('authRedirect', '1');
            window.location = onezoneUrl(
              `?redirect_url=${location.pathname}${location.hash}`
            );
          }
        });
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
  initSession(isPublic) {
    // bind session service websocket event handlers
    return this.get('server').initWebSocket(
      this.get('onWebSocketOpen'),
      this.get('onWebSocketError'),
      this.get('onWebSocketClose'),
      isPublic
    ).then(({ oneproviderOrigin, oneproviderToken }) => {
      safeExec(this, 'setProperties', {
        oneproviderOrigin,
        oneproviderToken,
      });
    });
  },

  /** Performs an RPC call and registers a promise that will resolve
   * client session when WebSocket is established and it has send session
   * details. */
  resolveSession() {
    console.debug('session.resolveSession');
    // Request session data
    // TODO: no reject handling
    return this.get('server').sessionRPC().then((data) => {
      console.debug("RESOLVE SESSION REQ");
      console.debug('data: ' + JSON.stringify(data));
      let isSessionValid = (data.sessionValid === true);
      this.set('sessionValid', isSessionValid);
      let promise;
      if (isSessionValid) {
        promise = this.onResolveSessionValid(data);
      } else {
        promise = this.onResolveSessionInvalid();
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

      return promise;
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
      }
    });
    sessionUserPromise.catch(() => {
      console.debug("SESSION: USER CANNOT BE FETCHED (findRecord rejected)");
      this.onResolveSessionInvalid();
    });
    return sessionUserPromise;
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
