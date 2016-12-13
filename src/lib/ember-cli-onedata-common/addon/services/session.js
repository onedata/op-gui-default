import Ember from 'ember';
import SessionCore from './session-core';

const RECONNECT_MSG_UPDATE_INTERVAL = 1000;
const FIRST_RECONNECT_INTERVAL = 5*1000;
const MAX_RECONNECT_INTERVAL = 60*1000;
const RECONNECTION_TIMEOUT = 30*1000;
const MAX_RECONNECT_TRIES = 10;

/**
 * An implementation of ember-simple-auth session service.
 * It extends core Onedata webgui session. You can provide either session-core
 * or session-core-no-simple-auth as a core session class.
 *
 * This module adds notifications about WS failures to user.
 * There are some modals that indicated reconnection: error message, countdown,
 * reconnecting, etc. - all these modals have modals metadata
 * ``{isReconnector: true}``
 *
 * @module services/session
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default SessionCore.extend({
  messageBox: Ember.inject.service(),
  i18n: Ember.inject.service(),
  browser: Ember.inject.service(),

  init() {
    this._super();
    this.setProperties({
      reconnectInterval: FIRST_RECONNECT_INTERVAL,
      reconnectionsCount: 0
    });
  },

  websocketWasOpened: false,

  /**
   * Produce a localized description message when WebSocket conntection is closed
   *
   * @param {CloseEvent} event An event passed to WebSocket onclose event handler
   *  See: https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
   * @param {unsigned short} event.code
   * @param {string} event.reason
   * @param {Boolean} event.wasClean
   * @returns {string} A message
   */
  wsCloseMessage(event) {
    switch (event.code) {
      case 1006:
        return this.get('i18n').t('services.session.connectionClosed.reasons.abnormal');
      default:
        return this.get('i18n').t('services.session.connectionClosed.reasons.unknown');
    }
  },

  /**
   * Returns a handler for WebSocket ``onclose`` event - will show a message.
   */
  onWebSocketClose: function() {
    return (event) => {
      this.set('websocketOpen', false);
      let ignore = false;
      let automaticReconnect = true;
      let message;

      if (!this.get('websocketWasOpened')) {
        automaticReconnect = false;
        message = this.get('i18n').t('services.session.connectionClosed.messageNotOpened');
        if (this.get('browser.browser.browserCode') === 'safari') {
          message += ': ' + this.get('i18n').t('services.session.connectionClosed.reasons.safariCert');
        }
      } else {
        // WebSocket.CLOSE_GOING_AWAY - used when user leaves current page
        if (event.code === 1001) {
          console.warn(`WebSocket has been closed because of WebSocket.CLOSE_GOING_AWAY`);
          ignore = true;
        }
        message = this.get('i18n').t('services.session.connectionClosed.message');
        message += ': ' + this.wsCloseMessage(event);
      }

      if (!ignore) {
        this.openConnectionClosedModal(message);

        if (automaticReconnect) {
          // Wait 5 seconds before starting reconnect modal
          setTimeout(() => this.startWebSocketReconnector(), 5*1000);
        }
      }
    };
  }.property(),

  openConnectionClosedModal(message) {
    this.get('messageBox').open({
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      type: 'error',
      allowClose: false,
      message: message
    });
  },

  timeToReconnect: null,

  openMaxTriesLimitModal() {
    this.get('messageBox').open({
      metadata: {isReconnector: true},
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      message: this.get('i18n').t('services.session.maxReconnectionsExceeded'),
      type: 'error',
      allowClose: false
    });
  },

  openCountdownModal() {
    this.get('messageBox').open({
      metadata: {isReconnector: true},
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      type: 'loading',
      allowClose: false
    });
  },

  updateCountdownModalTime() {
    let ttr = this.get('timeToReconnect');
    this.set(
      'messageBox.message',
      this.get('i18n').t(
        'services.session.connectionClosed.reconnectWait',
        {secs: ttr/1000}
      )
    );
    this.set('timeToReconnect', ttr - RECONNECT_MSG_UPDATE_INTERVAL);
  },

  startWebSocketReconnector() {
    if (this.get('reconnectionsCount') === MAX_RECONNECT_TRIES) {
      this.openMaxTriesLimitModal();
    } else {
      this.increaseReconnectInterval();
      this.set('timeToReconnect', this.get('reconnectInterval'));

      this.openCountdownModal();
      this.updateCountdownModalTime();
      const modalUpdaterInterval =
        setInterval(() => this.updateCountdownModalTime(), RECONNECT_MSG_UPDATE_INTERVAL);

      // reconnect after some time
      setTimeout(() => {
        clearInterval(modalUpdaterInterval);
        this.websocketReconnect();
      }, this.get('reconnectInterval'));
    }
  },

  increaseReconnectInterval() {
    let newReconnectInterval = this.get('reconnectInterval') * 2;
    if (newReconnectInterval > MAX_RECONNECT_INTERVAL) {
      newReconnectInterval = MAX_RECONNECT_INTERVAL;
    }
    this.set('reconnectInterval', newReconnectInterval);
  },

  openReconnectingModal() {
    this.get('messageBox').open({
      metadata: {isReconnector: true},
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      type: 'loading',
      allowClose: false,
      message: this.get('i18n').t('services.session.connectionClosed.reconnecting')
    });
  },

  websocketReconnect() {
    this.incrementProperty('reconnectionsCount');
    this.openReconnectingModal();

    this.get('server').clearWebsocket();
    this.get('server').initWebSocket(
      this.get('onWebSocketOpen'),
      this.get('onWebSocketError'),
      this.get('onWebSocketClose')
    );

    // set a timeout for reconnection
    const reconnectionTimeout = setTimeout(() => {
      this.get('server').closeWebsocket();
    }, RECONNECTION_TIMEOUT);
    // make sure there is no old reconnection timeouts
    clearTimeout(this.get('reconnectionTimeout'));
    this.set('reconnectionTimeout', reconnectionTimeout);
  },

  openReconnectSuccessModal() {
    this.set('websocketReconnectorOpen', false);
    this.get('messageBox').open({
      metadata: {modalId: 'reconnected-info', isReconnector: true},
      type: 'info',
      title: this.get('i18n').t('services.session.connectionReopened.title'),
      message: this.get('i18n').t('services.session.connectionReopened.message'),
      allowClose: true
    });
    setTimeout(() => {
      // ensue we close "connection reopened" message
      if (this.get('messageBox.metadata.modalId') === 'reconnected-info') {
        this.get('messageBox').close();
      }
    }, 5000);
  },

  resetReconnectionTries() {
    this.setProperties({
      reconnectInterval: FIRST_RECONNECT_INTERVAL,
      reconnectionsCount: 0,
    });
  },

  websocketOpenChanged: function() {
    if (this.get('messageBox.metadata.isReconnector')) {
      clearTimeout(this.get('reconnectionTimeout'));
      // the WS has been opened again
      if (this.get('websocketOpen')) {
        this.openReconnectSuccessModal();
        this.resetReconnectionTries();
      }
    }
  }.observes('websocketOpen'),

  websocketReconnectorOpenChanged: function() {
    if (!this.get('websocketReconnectorOpen')) {
      this.get('messageBox').close();
    }
  }.observes('websocketReconnectorOpen'),
});
