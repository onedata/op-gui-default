import Ember from 'ember';
import SessionCore from './session-core';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

const RECONNECT_MSG_UPDATE_INTERVAL = 1000;
const FIRST_RECONNECT_INTERVAL = 5 * 1000;
const MAX_RECONNECT_INTERVAL = 60 * 1000;
const RECONNECTION_TIMEOUT = 30 * 1000;
const MAX_RECONNECT_TRIES = 10;

const {
  Object: EmberObject,
  computed,
  String: { htmlSafe },
  observer,
  get,
  RSVP: { Promise, resolve },
  run: { later },
  inject: { service },
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
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default SessionCore.extend({
  i18n: service(),
  browser: service(),
  commonLoader: service(),
  store: service(),
  websocketConnection: service(),

  adapter: computed(function adapter() {
    return this.get('store').adapterFor('application');
  }),

  reconnectModal: EmberObject.create(),
  firstReconnect: true,

  /**
   * Max time to wait for pending operations on Websocket that will be closed
   * because of user request in milliseconds.
   * @type {number}
   */
  waitForPendingOperationsTimeout: 10000,

  init() {
    this._super();
    this.setProperties({
      reconnectInterval: FIRST_RECONNECT_INTERVAL,
      reconnectionsCount: 0
    });
  },

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
  onWebSocketClose: computed(function () {
    let i18n = this.get('i18n');
    return (event) => {
      this.set('websocketOpen', false);
      this.set('websocketWasClosed', true);
      let message;
      let isSafari = (this.get('browser.browser.browserCode') === 'safari');

      if (!this.get('websocketWasOpened')) {
        message = i18n.t('services.session.connectionClosed.messageNotOpened');
        if (isSafari) {
          message += ': ' + i18n.t(
            'services.session.connectionClosed.reasons.safariCert');
        }
        this.openConnectionClosedModal(message);
        const commonLoader = this.get('commonLoader');
        if (get(commonLoader, 'type') === 'login') {
          commonLoader.setProperties({
            isLoading: false,
            message: null,
            messageSecondary: null,
            type: null,
          });
        }
      } else {
        // WebSocket.CLOSE_GOING_AWAY - used when user leaves current page
        if (event.code === 1001) {
          console.warn(
            `WebSocket has been closed because of WebSocket.CLOSE_GOING_AWAY`);
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
      mode: 'cannotOpen',
      allowClose: false,
      message: message
    });
  },

  openMaxTriesLimitModal() {
    this.get('reconnectModal').setProperties({
      open: true,
      metadata: { isReconnector: true },
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      message: this.get('i18n').t('services.session.maxReconnectionsExceeded'),
      mode: 'limitExceeded',
      type: 'error',
      allowClose: false
    });
  },

  openCountdownModal() {
    this.get('reconnectModal').setProperties({
      open: true,
      type: 'loading',
      mode: 'waiting',
    });
  },

  openReconnectingModal() {
    this.get('reconnectModal').setProperties({
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      type: 'loading',
      mode: 'reconnecting',
      message: this.get('i18n').t('services.session.connectionClosed.reconnecting')
    });
  },

  updateCountdownModalTime(message) {
    let {
      i18n,
      timeToReconnect: ttr
    } = this.getProperties('i18n', 'timeToReconnect');
    let reasonMsg = message;
    let delayMsg = i18n.t(
      'services.session.connectionClosed.reconnectWait', { secs: ttr / 1000 }
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
      let reconnectInterval = this.get('reconnectInterval');
      this.set('timeToReconnect', reconnectInterval);

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
        this.websocketReconnect({
          isPublic: this.get('websocketConnection.currentConnectionIsPublic'),
        });
      }, reconnectInterval);

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
    let isSafari = (this.get('browser.browser.browserCode') === 'safari');
    let newReconnectInterval;

    // workaround for "back" issue in Safari
    if (isSafari && this.get('firstReconnect') && this.get('sessionValid') === false) {
      newReconnectInterval = 0;
    } else {
      let reconnectInterval = this.get('reconnectInterval');
      newReconnectInterval = reconnectInterval ? reconnectInterval * 2 :
        FIRST_RECONNECT_INTERVAL;
      if (newReconnectInterval > MAX_RECONNECT_INTERVAL) {
        newReconnectInterval = MAX_RECONNECT_INTERVAL;
      }
    }
    this.set('firstReconnect', false);
    this.set('reconnectInterval', newReconnectInterval);
  },

  websocketReconnect({ isPublic, onDemand }) {
    const server = this.get('server');

    if (!onDemand) {
      this.incrementProperty('reconnectionsCount');
      this.openReconnectingModal();
    }

    let waitAndClose;

    if (onDemand) {
      waitAndClose = this.waitForPendingOperations();
    } else {
      waitAndClose = resolve();
    }

    const reconnectingPromise = new Promise((resolve, reject) => {
      waitAndClose
        .then(() => server.clearWebsocket())
        .then(() => {
          server.initWebSocket(
            this.get('onWebSocketOpen'),
            this.get('onWebSocketError'),
            this.get('onWebSocketClose'),
            isPublic
          ).then(({ oneproviderApiOrigin, oneproviderToken }) => {
            safeExec(this, 'setProperties', {
              oneproviderToken,
              oneproviderApiOrigin,
            });
            console.debug('WebSocket reconnected successfully');
            resolve();
          });
        });

      // set a timeout for reconnection
      const reconnectionTimeout = setTimeout(() => {
        this.get('server').closeWebsocket();
        reject();
      }, RECONNECTION_TIMEOUT);
      // make sure there is no old reconnection timeouts
      clearTimeout(this.get('reconnectionTimeout'));
      this.set('reconnectionTimeout', reconnectionTimeout);
    });

    this.set('reconnectingPromise', reconnectingPromise);
    return reconnectingPromise;
  },

  resetReconnectionTries() {
    this.setProperties({
      reconnectInterval: FIRST_RECONNECT_INTERVAL,
      reconnectionsCount: 0,
    });
  },

  reconnectNow() {
    this.stopAutoReconnector();
    return this.websocketReconnect({
      isPublic: this.get('websocketConnection.currentConnectionIsPublic'),
    });
  },

  waitForPendingOperations() {
    const adapter = this.get('adapter');
    const waitForPendingOperationsTimeout = this.get('waitForPendingOperationsTimeout');
    if (get(adapter, 'isRespSemaphoreAcquired')) {
      return new Promise((resolve) => {
        let laterId;
        const onRespSemaphoreReleased = () => {
          adapter.off('respSemaphoreReleased', onRespSemaphoreReleased);
          clearTimeout(laterId);
          resolve();
        };
        adapter.on('respSemaphoreReleased', onRespSemaphoreReleased);
        laterId = later(onRespSemaphoreReleased, waitForPendingOperationsTimeout);
      });
    } else {
      return resolve();
    }
  },

  websocketOpenChanged: observer('websocketOpen', function () {
    clearTimeout(this.get('reconnectionTimeout'));
    // the WS has been opened again
    if (this.get('websocketWasClosed') && this.get('websocketOpen')) {
      this.set('websocketWasClosed', false);
      this.resetReconnectionTries();
      this.set('reconnectModal', Ember.Object.create());
      if (this.get('sessionValid') === false) {
        window.location.reload();
      }
    }
  }),
});
