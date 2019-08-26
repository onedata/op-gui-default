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
  get,
  RSVP: { Promise, resolve, reject },
  run: { debounce },
  inject: { service },
  computed,
  run: { next },
} = Ember;

// Path where WS server is hosted
const wsEndpoint = '/ws';

const flushTimeout = 20;

/**
 * Interval for checking if token expired and WS connection should be restarted
 * @type {number} in milliseconds
 */
const checkExpirationIntervalTime = 20000;

function getApiCredentials(isPublic = false) {
  return (isPublic ? resolve() : getApiToken())
    .then(tokenData => {
      const token = tokenData && tokenData.token;
      const ttl = tokenData && tokenData.ttl;
      return resolve($.ajax('./gui-context'))
        .then(({ apiOrigin }) => ({
          token,
          ttl,
          apiOrigin
        }));
    });
}

function getApiToken() {
  return resolve($.post('./gui-preauthorize'))
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
  session: service(),
  fileUpload: service(),

  initialized: false,
  onOpenCallback: null,
  onErrorCallback: null,
  onCloseCallback: null,

  adapter: computed(function adapter() {
    return this.get('store').adapterFor('application');
  }),

  /**
   * @type {number}
   */
  secondsBeforeExpire: undefined,

  /**
   * @type {Date}
   */
  tokenExpireDate: undefined,

  /**
   * Id of interval for checking if token expired
   * @type {number}
   */
  checkExpirationInterval: undefined,

  init() {
    this._super(...arguments);
    setInterval(() => {
      this.reconnectIfNeeded();
    }, checkExpirationIntervalTime);

  },

  destroy() {
    try {
      clearInterval(this.get('checkExpirationInterval'));
    } finally {
      this._super(...arguments);
    }
  },

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

  reconnectIfNeeded() {
    const {
      socket,
      tokenExpireDate,
      fileUpload,
      currentConnectionIsPublic,
      store,
    } = this.getProperties(
      'socket',
      'tokenExpireDate',
      'fileUpload',
      'currentConnectionIsPublic',
      'store'
    );
    console.debug('Checking if session token needs refresh...');
    const uploadInProgress = get(fileUpload, 'uploadInProgress');
    if (socket && socket.readyState === WebSocket.OPEN && !
      uploadInProgress && tokenExpireDate && Date.now() >= tokenExpireDate) {
      console.debug('Will refresh session token');
      return this.get('session').websocketReconnect({
          isPublic: currentConnectionIsPublic,
          onDemand: true,
        })
        .then(() => {
          store
            .peekAll('file')
            .filterBy('isDir')
            .map(f => f.reload());
        });
    } else {
      console.debug('Not performing token refresh');
      return resolve();
    }
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

      const protocol = window.location.protocol === 'https:' ? 'wss://' : 'ws://';

      // TODO: if getOneproviderToken fail, we will see infinite loading
      return getApiCredentials(isPublic)
        .then(({ token: oneproviderToken, apiOrigin: oneproviderApiOrigin, ttl }) => {
          let url = `${protocol}${oneproviderApiOrigin}${wsEndpoint}`;
          const secondsBeforeExpire = this.set('secondsBeforeExpire', ttl / 2);
          const tokenExpireDate = this.set(
            'tokenExpireDate',
            new Date(Date.now() + ttl * 1000 - secondsBeforeExpire * 1000)
          );

          console.debug('Next token refresh will be done after: ' + tokenExpireDate);

          if (oneproviderToken) {
            url += `?token=${oneproviderToken}`;
          }

          console.debug('Connecting: ' + url);

          return new Promise((resolveWS, rejectWS) => {
            if (this.get('socket') === null) {
              try {
                const socket = this.set('socket', new WebSocket(url));
                socket.onopen = (event) => {
                  this.open(event)
                    .then(() => resolveWS({
                      oneproviderApiOrigin,
                      oneproviderToken
                    }));
                };
                socket.onmessage = (event) => {
                  this.receive(event);
                };
                socket.onerror = (event) => {
                  this.error(event);
                  rejectWS();
                };
                socket.onclose = (event) => {
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
    const socket = this.get('socket');
    return new Promise(resolve => {
      if (socket && socket.readyState !== WebSocket.CLOSED) {
        socket.onclose = () => resolve();
        socket.close();
      } else {
        return resolve();
      }
    }).then(() => this.set('initialized', false));
  },

  clearWebsocket() {
    return this.closeWebsocket()
      .then(() => {
        this.set('socket', null);
      });
  },
});
