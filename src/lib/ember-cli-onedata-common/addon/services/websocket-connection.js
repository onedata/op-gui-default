/**
 * Operations on WebSocket level (connect, disconnect, events handling etc.)
 * 
 * @module services/websocket-connection
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import onezoneUrl from 'op-worker-gui/utils/onezone-url';

const {
  Service,
  RSVP: { Promise, resolve, reject },
  run: { debounce },
  inject: { service },
  computed,
  run: { next },
} = Ember;

// Path where WS server is hosted
const wsEndpoint = '/ws';

const flushTimeout = 20;

function getApiCredentials(isPublic = false) {
  return (isPublic ? resolve() : getApiToken())
    .then(tokenData => {
      const token = tokenData && tokenData.token;
      return resolve($.ajax('./gui-context'))
        .then(({ origin }) => ({
          token,
          origin
        }));
    });
}

const reInOnzoneUrl = /.*\/(opw)\/(.*?)\/(.*)/;

function getApiToken() {
  return resolve($.ajax('./gui-preauthorize'))
    .catch(() => {
      next(() => {
        if (!window.onedataIsReloadingApp) {
          sessionStorage.setItem('authRedirect', '1');
          window.location = onezoneUrl(
            `?redirect_url=${location.pathname}${location.hash}`
          );
        }
      });
    });
}

export default Service.extend({
  store: service(),

  initialized: false,
  onOpenCallback: null,
  onErrorCallback: null,
  onCloseCallback: null,

  adapter: computed(function adapter() {
    return this.get('store').adapterFor('application');
  }),

  /** WebSocket onopen callback */
  open(event) {
    const onOpen = this.get('onOpenCallback');
    // Flush messages waiting for connection open
    const adapter = this.get('adapter');
    debounce(() => adapter.flushMessageBuffer(), flushTimeout);
    if (onOpen) {
      return onOpen(event);
    } else {
      return resolve();
    }
  },

  /** WebSocket onerror callback */
  error(event) {
    // TODO @todo better error handling, maybe reconnection attempts?
    console.error(`WebSocket connection error, event: ` + JSON.stringify(event));

    const onError = this.get('onErrorCallback');
    if (onError) {
      onError(event);
    }
  },

  /** WebSocket onclose callback */
  close(event) {
    console.error(`WebSocket connection closed, event: ` +
      `code: ${event.code}, reason: ${event.reason}, wasClean: ${event.wasClean}`);

    const onClose = this.get('onCloseCallback');
    if (onClose) {
      onClose(event);
    }
  },

  getClusterIdFromUrl() {
    const m = location.toString().match(reInOnzoneUrl);
    return m && m[2];
  },

  /**
   * Initializes the WebSocket
   */
  initWebSocket(onOpen, onError, onClose, isPublic = false) {
    // Register callbacks even if WebSocket is already being initialized.
    if (onOpen) {
      this.set('onOpenCallback', onOpen);
    }
    if (onError) {
      this.set('onErrorCallback', onError);
    }
    if (onClose) {
      this.set('onCloseCallback', onClose);
    }
    if (this.get('initialized') === true) {
      return reject();
    } else {
      this.set('initialized', true);

      let protocol = window.location.protocol === 'https:' ? 'wss://' : 'ws://';

      const clusterId = this.getClusterIdFromUrl();
      // TODO: if getOneproviderToken fail, we will see infinite loading
      return getApiCredentials(isPublic)
        .then(({ token: oneproviderToken, origin: oneproviderOrigin }) => {
          let url = oneproviderOrigin + wsEndpoint;

          if (oneproviderToken) {
            url += `?token=${oneproviderToken}`;
          }

          console.debug('Connecting: ' + url);

          return new Promise((resolveWS, rejectWS) => {
            if (this.socket === null) {
              try {
                this.socket = new WebSocket(url);
                this.socket.onopen = (event) => {
                  this.open(event)
                    .then(() => resolveWS({
                      oneproviderOrigin,
                      oneproviderToken
                    }));
                };
                this.socket.onmessage = (event) => {
                  this.receive(event);
                };
                this.socket.onerror = (event) => {
                  this.error(event);
                  rejectWS();
                };
                this.socket.onclose = (event) => {
                  this.close(event);
                };
              } catch (error) {
                console.error(`WebSocket initializtion exception: ${error}`);
                // invoke provided handler, it should idicate error to user
                onClose();
                throw error;
              }
            }
          });
        });
    }
  },

  /** WebSocket onmessage callback, resolves promises with received replies. */
  receive(event) {
    let json = JSON.parse(event.data);
    if (Array.isArray(json.batch)) {
      // as the for..of loop is currently tanspiled with Babel
      // this can slightly increase performance
      // and facilitates debugging
      const adapter = this.get('adapter');
      if (json.batch.length === 1) {
        adapter.processMessage(json.batch[0]);
      } else {
        for (let message of json.batch) {
          adapter.processMessage(message);
        }
      }
    } else {
      console.warn(
        'A json.batch message was dropped because is not an Array, see debug logs for details'
      );
      console.debug('Warning: dropping message: ' + JSON.stringify(json));
    }
  },

  closeWebsocket() {
    return new Promise(resolve => {
      if (this.socket) {
        this.socket.onclose = () => resolve();
        this.socket.close();
      } else {
        return resolve();
      }
    });
  },

  clearWebsocket() {
    return this.closeWebsocket()
      .then(() => {
        this.setProperties({
          socket: null,
          initialized: false
        });
      });
  },
});
