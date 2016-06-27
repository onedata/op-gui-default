import Ember from 'ember';
import SessionCore from './session-core';

const RECONNECT_MSG_UPDATE_INTERVAL = 1000;
const FIRST_RECONNECT_INTERVAL = 5*1000;

/**
 * An implementation of ember-simple-auth session service.
 * It extends core Onedata webgui session. You can provide either session-core
 * or session-core-no-simple-auth as a core session class.
 *
 * This module adds notifications about WS failures to user.
 * @module services/session
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default SessionCore.extend({
  messageBox: Ember.inject.service(),
  i18n: Ember.inject.service(),
  browser: Ember.inject.service(),

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
      let automaticReconnect = true;
      let message = this.get('i18n').t('services.session.connectionClosed.message');


      // 1006 code on Safari is probably a certificate error
      if (event.code === 1006 &&
        this.get('browser.browser.browserCode') === 'safari') {
          automaticReconnect = false;
          message += ': ' + this.get('i18n').t('services.session.connectionClosed.reasons.safariCert');
        } else {
          message += ': ' + this.wsCloseMessage(event);
        }

      this.get('messageBox').open({
        title: this.get('i18n').t('services.session.connectionClosed.title'),
        type: 'error',
        allowClose: false,
        message: message
      });

      if (automaticReconnect) {
        // Wait 5 seconds before starting reconnect modal
        setTimeout(() => this.startWebSocketReconnector(), 5*1000);
      }
    };
  }.property(),

  timeToReconnect: null,

  startWebSocketReconnector() {
    this.set('timeToReconnect', FIRST_RECONNECT_INTERVAL);

    this.get('messageBox').open({
      title: this.get('i18n').t('services.session.connectionClosed.title'),
      type: 'loading',
      allowClose: false
    });

    const updateModalTime = () => {
      let ttr = this.get('timeToReconnect');
      this.set(
        'messageBox.message',
        this.get('i18n').t(
          'services.session.connectionClosed.reconnectWait',
          {secs: ttr/1000}
        )
      );
      this.set('timeToReconnect', ttr - RECONNECT_MSG_UPDATE_INTERVAL);
    };

    updateModalTime();
    const intervalId = setInterval(updateModalTime, RECONNECT_MSG_UPDATE_INTERVAL);

    setTimeout(() => {
      clearInterval(intervalId);

      this.get('messageBox').open({
        title: this.get('i18n').t('services.session.connectionClosed.title'),
        type: 'loading',
        allowClose: false,
        message: this.get('i18n').t('services.session.connectionClosed.reconnecting')
      });

      // TODO: should reconnect only websocket, not whole app?
      setTimeout(() => window.location.reload(), 500);

    }, FIRST_RECONNECT_INTERVAL);
  }
});
