import Ember from 'ember';
import BaseSession from './base-session';

const RECONNECT_MSG_UPDATE_INTERVAL = 1000;
const FIRST_RECONNECT_INTERVAL = 5*1000;

// FIXME doc
// FIXME move this file to gui
export default BaseSession.extend({
  messageBox: Ember.inject.service(),
  i18n: Ember.inject.service(),

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
      let message = this.get('i18n').t('services.session.connectionClosed.message');
      message += ': ' + this.wsCloseMessage(event);
      this.get('messageBox').open({
        title: this.get('i18n').t('services.session.connectionClosed.title'),
        type: 'error',
        allowClose: false,
        message: message
      });

      // Wait 5 seconds before starting reconnect modal
      setTimeout(() => this.startWebSocketReconnector(), 5*1000);
    };
  }.property(),

  // TODO: reset to 5 secs then successfully connected (onopen)
  nextReconnectInterval: FIRST_RECONNECT_INTERVAL,
  timeToReconnect: null,

  startWebSocketReconnector() {
    this.set('timeToReconnect', this.get('nextReconnectInterval'));

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

      // TODO: should reconnect only websocket
      setTimeout(() => window.location.reload(), 500);

      // TODO: reconnect with only websocket and next reconnect interval
      // this.initSession();
      // if (reconnectedFailed) {
      //   let nextInterval = this.get('nextReconnectIntervalSecs');
      //   if (nextInterval > 60) {
      //     nextInterval = 60;
      //   }
      //   this.set('nextReconnectInterval', nextInterval);
      // } else {
      //   this.set('nextReconnectInterval', FIRST_RECONNECT_INTERVAL);
      // }


    }, this.get('nextReconnectInterval'));
  }
});
