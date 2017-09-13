import Ember from 'ember';
import SessionCore from './session-core';

const RECONNECT_MSG_UPDATE_INTERVAL = 1000;
const FIRST_RECONNECT_INTERVAL = 5*1000;
const MAX_RECONNECT_INTERVAL = 60*1000;
const RECONNECTION_TIMEOUT = 30*1000;
const MAX_RECONNECT_TRIES = 10;

const {
  computed,
  String: { htmlSafe },
  observer,
} = Ember;

/**
 * An implementation of ember-simple-auth session service.
 * It extends core Onedata webgui session.
 *
 * This module adds notifications about WS failures to user.
 * There are some modals that indicated reconnection: error message, countdown,
 * reconnecting, etc. - all these modals have modals metadata
 * `{isReconnector: true}`
 *
 * @module services/session
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default SessionCore.extend({
  i18n: Ember.inject.service(),
  browser: Ember.inject.service(),

  reconnectModal: Ember.Object.create(),
    
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
   * Returns a handler for WebSocket `onclose` event - will show a message.
   */
  onWebSocketClose: computed(function() {
    let i18n = this.get('i18n');
    return (event) => {
      this.set('websocketOpen', false);
      let automaticReconnect = true;
      let message;

      if (!this.get('websocketWasOpened')) {
        automaticReconnect = false;
        message = i18n.t('services.session.connectionClosed.messageNotOpened');
        if (this.get('browser.browser.browserCode') === 'safari') {
          message += ': ' + i18n.t('services.session.connectionClosed.reasons.safariCert');
        }
        this.openConnectionClosedModal(message);
      } else {
        // WebSocket.CLOSE_GOING_AWAY - used when user leaves current page
        if (event.code === 1001) {
          console.warn(`WebSocket has been closed because of WebSocket.CLOSE_GOING_AWAY`);
        } else {
          message =
            i18n.t('services.session.connectionClosed.message') +
            ': ' + this.wsCloseMessage(event);
          this.startWebSocketReconnector(message);
        }
      }
    };
  }),

  openConnectionClosedModal(message) {
    this.get('reconnectModal').setProperties({
      open: true,
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      type: 'error',
      allowClose: false,
      message: message
    });
  },
  
  openMaxTriesLimitModal() {
    this.get('reconnectModal').setProperties({
      open: true,
      metadata: {isReconnector: true},
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      message: this.get('i18n').t('services.session.maxReconnectionsExceeded'),
      type: 'error',
      allowClose: false
    });
  },

  openCountdownModal() {
    this.get('reconnectModal').setProperties({
      open: true,
      mode: 'waiting',
    });
  },

  updateCountdownModalTime(message) {
    let {
      i18n,
      timeToReconnect: ttr
    } = this.getProperties('i18n', 'timeToReconnect');
    let reasonMsg = message;
    let delayMsg = i18n.t(
      'services.session.connectionClosed.reconnectWait',
      {secs: ttr/1000}
    );
    this.set(
      'reconnectModal.message',
      htmlSafe(`${reasonMsg}<br>${delayMsg}`)
    );
    this.set('timeToReconnect', ttr - RECONNECT_MSG_UPDATE_INTERVAL);
  },

  startWebSocketReconnector(message) {
    if (this.get('reconnectionsCount') === MAX_RECONNECT_TRIES) {
      this.openMaxTriesLimitModal();
    } else {
      this.increaseReconnectInterval();
      this.set('timeToReconnect', this.get('reconnectInterval'));

      this.openCountdownModal();
      this.updateCountdownModalTime(message);
      const modalUpdaterInterval = setInterval(
        () => this.updateCountdownModalTime(message),
        RECONNECT_MSG_UPDATE_INTERVAL
      );

      this.set('modalUpdaterInterval', modalUpdaterInterval);
      
      // reconnect after some time
      let reconnectTryTimeout = setTimeout(() => {
        clearInterval(modalUpdaterInterval);
        this.websocketReconnect();
      }, this.get('reconnectInterval'));
      
      this.set('reconnectTryTimeout', reconnectTryTimeout);
    }
  },

  stopAutoReconnector() {
    let {
      modalUpdaterInterval,
      reconnectTryTimeout,
    } = this.getProperties('modalUpdaterInterval', 'reconnectTryTimeout');
    clearInterval(modalUpdaterInterval);
    clearTimeout(reconnectTryTimeout);
  },
  
  increaseReconnectInterval() {
    let newReconnectInterval = this.get('reconnectInterval') * 2;
    if (newReconnectInterval > MAX_RECONNECT_INTERVAL) {
      newReconnectInterval = MAX_RECONNECT_INTERVAL;
    }
    this.set('reconnectInterval', newReconnectInterval);
  },

  openReconnectingModal() {
    this.get('reconnectModal').setProperties({
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      type: 'loading',
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

  resetReconnectionTries() {
    this.setProperties({
      reconnectInterval: FIRST_RECONNECT_INTERVAL,
      reconnectionsCount: 0,
    });
  },

  reconnectNow() {
    this.stopAutoReconnector();
    return this.websocketReconnect();
  },
  
  websocketOpenChanged: observer('websocketOpen', function() {
    clearTimeout(this.get('reconnectionTimeout'));
    // the WS has been opened again
    if (this.get('websocketOpen')) {
      this.resetReconnectionTries();
      this.set('reconnectModal', Ember.Object.create());
      if (this.get('sessionValid') === false) {
        window.location.reload();
      }
    }
  }),
});
