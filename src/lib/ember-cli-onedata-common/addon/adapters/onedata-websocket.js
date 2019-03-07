/**
 * Custom adapter that handles model synchronization between client and server
 * using a websocket connection.
 * @module adapters/application
 * @author Jakub Liput, Łukasz Opioła
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

// This file should be linked to app/adapters/application.js

import Ember from 'ember';
import DS from 'ember-data';

const {
  RSVP: { resolve, reject, Promise }
} = Ember;

/** -------------------------------------------------------------------
 * Interface between client and server
 * Corresponding interface is located in gui_ws_handler.erl.
 * ------------------------------------------------------------------- */
// Path where WS server is hosted
let WS_ENDPOINT = '/ws';
// Flush timeout of batch requests - they are accumulated and if no new request
// from ember store comes within this time, the batch is sent to the server.
// Expressed in milliseconds.
let FLUSH_TIMEOUT = 20;

// All out-coming JSONs have the following structure (opt = optional field)
// {
//   uuid
//   msgType
//   resourceType
//   operation
//   resourceIds (opt)
//   data (opt)
// }
// All in-coming JSONs have the following structure (opt = optional field)
// {
//   uuid (opt, not used in push messages)
//   msgType
//   result (opt, not used in push messages)
//   data (opt)
// }

// Message types, identified by `msgType` key
let TYPE_MODEL_REQ = 'modelReq';
let TYPE_MODEL_RESP = 'modelResp';
let TYPE_MODEL_CRT_PUSH = 'modelPushCreated';
let TYPE_MODEL_UPT_PUSH = 'modelPushUpdated';
let TYPE_MODEL_DLT_PUSH = 'modelPushDeleted';
let TYPE_RPC_REQ = 'RPCReq';
let TYPE_RPC_RESP = 'RPCResp';
let TYPE_PUSH_MESSAGE = 'pushMessage';
// Operations on model, identified by `operation` key
let OP_FIND_RECORD = 'findRecord';
let OP_FIND_ALL = 'findAll';
let OP_QUERY = 'query';
let OP_QUERY_RECORD = 'queryRecord';
let OP_FIND_MANY = 'findMany';
let OP_FIND_HAS_MANY = 'findHasMany';
let OP_FIND_BELONGS_TO = 'findBelongsTo';
let OP_CREATE_RECORD = 'createRecord';
let OP_UPDATE_RECORD = 'updateRecord';
let OP_DELETE_RECORD = 'deleteRecord';
// Operation results, identified by `result` key
let RESULT_OK = 'ok';
let RESULT_ERROR = 'error';

const FETCH_MODEL_OPERATIONS = new Set([
  OP_FIND_RECORD,
  OP_FIND_ALL,
  OP_QUERY,
  OP_QUERY_RECORD,
  OP_FIND_MANY,
  OP_CREATE_RECORD
]);

const reInOnzoneUrl = /.*\/(op)\/(.*?)\/(.*)/;

function getApiToken(clusterId) {
  return new Promise((resolve, reject) => $.ajax(
      `/gui-token`, {
        method: 'POST',
        contentType: 'application/json; charset=utf-8',
        dataType: 'json',
        data: JSON.stringify({
          clusterId,
          clusterType: 'oneprovider',
        }),
      }
    ).then(resolve, reject))
    .catch(error => {
      if (error && error.status === 401) {
        return new Promise(() => {
          if (sessionStorage.getItem('redirectFromOnezone') === 'true') {
            sessionStorage.setItem('redirectFromOnezone', 'false');
            throw new Error(
              'Redirection loop detected, try to clear browser cookies, logout from Onezone or contact administrators.'
            );
          } else {
            window.location =
              `/oz/onezone/i#/?redirect_url=${location.pathname}${location.hash}`;
          }
        });
      } else {
        throw error;
      }
    });
}

function getApiCredentials(clusterId, isPublic = false) {
  return (isPublic ? resolve() : getApiToken(clusterId))
    .then(tokenData => {
      const token = tokenData && tokenData.token;
      return new Promise((resolve, reject) => $.ajax(
          `/gui-origin`, {
            method: 'POST',
            contentType: 'application/json; charset=utf-8',
            dataType: 'json',
            data: JSON.stringify({
              clusterId,
              clusterType: 'oneprovider',
            }),
          }
        ).then(resolve, reject))
        .then(({ domain }) => ({
          token,
          domain
        }));
    });
}

export default DS.RESTAdapter.extend({
  store: Ember.inject.service('store'),
  serverMessagesHandler: Ember.inject.service(),

  initialized: false,
  onOpenCallback: null,
  onErrorCallback: null,
  onCloseCallback: null,

  shouldBackgroundReloadRecord() {
    return false;
  },

  shouldReloadRecord() {
    return false;
  },

  //
  /**
   * Map of promises that will be resolved when response for message with
   * specified uuid comes.
   * Adding of values to this map is done in ``sendAndRegisterPromise``.
   * The handling of responses is done in ``processMessage``.
   *
   * Maps ``uuid -> PromiseSpec``
   * where PromiseSpec is an object with properties:
   *
   * - ``success``: a function that will be executed on receive of message with the uuid
   *   and success result (see processMessage)
   * - ``error``: a function that will be executed on receive of message with the uuid
   *   and error result (see processMessage)
   * - ``type``: a string indicating message type - see TYPE_* constants in this file
   * - ``operation``: a string indicating model operation type - see OP_* constants
   */
  promises: new Map(),

  // The WebSocket
  socket: null,
  // Queue of messages. They are accumulated if requests from store come
  // frequently and flushed after FLUSH_TIMEOUT.
  messageBuffer: [],

  /** -------------------------------------------------------------------
   * WebSocket operations
   * ------------------------------------------------------------------- */

  getClusterIdFromUrl() {
    const m = location.toString().match(reInOnzoneUrl);
    return m && m[2];
  },

  /** Initializes the WebSocket */
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
      let adapter = this;

      let protocol = window.location.protocol === 'https:' ? 'wss://' : 'ws://';

      const clusterId = this.getClusterIdFromUrl();
      // TODO: if getOneproviderToken fail, we will see infinite loading
      return getApiCredentials(clusterId, isPublic)
        .then(({ token: oneproviderToken, domain: oneproviderHostname }) => {
          const port = window.location.port;
          let url = protocol + oneproviderHostname +
            (port === '' ? '' : ':' + port) + WS_ENDPOINT;

          if (oneproviderToken) {
            url += `?token=${oneproviderToken}`;
          }

          console.debug('Connecting: ' + url);

          if (adapter.socket === null) {
            try {
              adapter.socket = new WebSocket(url);
              adapter.socket.onopen = function (event) {
                adapter.open.apply(adapter, [event]);
              };
              adapter.socket.onmessage = function (event) {
                adapter.receive.apply(adapter, [event]);
              };
              adapter.socket.onerror = function (event) {
                adapter.error.apply(adapter, [event]);
              };
              adapter.socket.onclose = function (event) {
                adapter.close.apply(adapter, [event]);
              };
            } catch (error) {
              console.error(`WebSocket initializtion exception: ${error}`);
              // invoke provided handler, it should idicate error to user
              onClose();
              throw error;
            }
          }

          return resolve({ oneproviderHostname, oneproviderToken });
        });
    }
  },

  closeWebsocket() {
    if (this.socket) {
      this.socket.close();
    }
  },

  clearWebsocket() {
    this.closeWebsocket();
    this.setProperties({
      socket: null,
      initialized: false
    });
  },

  /** -------------------------------------------------------------------
   * Adapter API
   * ------------------------------------------------------------------- */

  /** Developer function - for logging/debugging */
  logToConsole(fun_name, fun_params) {
    console.debug(fun_name + '(');
    if (fun_params) {
      for (let i = 0; i < fun_params.length; i++) {
        console.debug('    ' + String(fun_params[i]));
      }
    }
    console.debug(')');
  },

  /** Called when ember store wants to find a record */
  findRecord(store, type, id, record) {
    this.logToConsole(OP_FIND_RECORD, [store, type, id, record]);
    return this.asyncRequest(OP_FIND_RECORD, type.modelName, id);
  },

  /** Called when ember store wants to find all records of a type */
  findAll(store, type, sinceToken) {
    this.logToConsole(OP_FIND_ALL, [store, type, sinceToken]);
    return this.asyncRequest(OP_FIND_ALL, type.modelName, null, sinceToken);
  },

  /** Called when ember store wants to find all records that match a query */
  query(store, type, query) {
    this.logToConsole(OP_QUERY, [store, type, query]);
    return this.asyncRequest(OP_QUERY, type.modelName, null, query);
  },

  queryRecord(store, type, query) {
    this.logToConsole(OP_QUERY_RECORD, [store, type, query]);
    return this.asyncRequest(OP_QUERY_RECORD, type.modelName, null, query);
  },

  /** Called when ember store wants to find multiple records by id */
  findMany(store, type, ids, records) {
    this.logToConsole(OP_FIND_MANY, [store, type, ids, records]);
    return this.asyncRequest(OP_FIND_MANY, type.modelName, null, ids);
  },

  /** @todo is this needed? **/
  findHasMany(store, record, url, relationship) {
    this.logToConsole(OP_FIND_HAS_MANY, [store, record, url, relationship]);
    return 'not_implemented';
  },

  /** @todo is this needed? */
  findBelongsTo(store, record, url, relationship) {
    this.logToConsole(OP_FIND_BELONGS_TO, [store, record, url, relationship]);
    return 'not_implemented';
  },

  /** Called when ember store wants to create a record */
  createRecord(store, type, record) {
    this.logToConsole(OP_CREATE_RECORD, [store, type, record]);
    let data = {};
    let serializer = store.serializerFor(type.modelName);
    serializer.serializeIntoHash(data, type, record, { includeId: true });
    return this.asyncRequest(OP_CREATE_RECORD, type.modelName, null, data);
  },

  /** Called when ember store wants to update a record */
  updateRecord(store, type, snapshot) {
    this.logToConsole(OP_UPDATE_RECORD, [store, type, snapshot]);
    let id = Ember.get(snapshot, 'id');

    let changedAttributes = snapshot.changedAttributes();
    let keys = Object.keys(changedAttributes);

    const serializer = store.serializerFor(type.modelName);
    let changesData =
      serializer.serialize(snapshot, { keys: keys });

    return this.asyncRequest(OP_UPDATE_RECORD, type.modelName, id, changesData);
  },

  /** Called when ember store wants to delete a record */
  deleteRecord(store, type, record) {
    this.logToConsole(OP_DELETE_RECORD, [store, type, record]);
    let id = Ember.get(record, 'id');
    return this.asyncRequest(OP_DELETE_RECORD, type.modelName, id);
  },

  /** @todo is this needed? */
  groupRecordsForFindMany(store, records) {
    this.logToConsole('groupRecordsForFindMany', [store, records]);
    return [records];
  },

  /** -------------------------------------------------------------------
   * RPC API
   * ------------------------------------------------------------------- */

  /**
   * Calls back to the server. Useful for getting information like
   * user name etc. from the server or performing some operations that
   * are not model-based.
   * @param {string} type - identifier of resource, e.g. 'public' for public RPC
   * @param {string} operation - function identifier
   * @param {object} data - json data
   */
  RPC(type, operation, data) {
    this.logToConsole('RPC', [type, operation, JSON.stringify(data)]);
    let payload = {
      msgType: TYPE_RPC_REQ,
      resourceType: type,
      operation: operation,
      data: data
    };
    this.respSemaphoreAcquire();
    return this.sendAndRegisterPromise(operation, type, payload);
  },

  /** -------------------------------------------------------------------
   * Semaphore to not allow pushing before all model resp are done
   * ------------------------------------------------------------------- */

  _respSemaphore: 0,

  waitingMessages: Ember.A(),

  isRespSemaphoreAcquired: Ember.computed('_respSemaphore', function () {
    return this.get('_respSemaphore') > 0;
  }),

  respSemaphoreAcquire() {
    this.incrementProperty('_respSemaphore');
  },

  respSemaphoreRelease() {
    this.decrementProperty('_respSemaphore');
    if (this.get('_respSemaphore') < 0) {
      this.set('_respSemaphore', 0);
    }
  },

  /** -------------------------------------------------------------------
   * Internal functions
   * ------------------------------------------------------------------- */

  /**
   * Performs an sync request to server side and stores a handle to the
   * promise, which will be resolved in receive function.
   */
  asyncRequest(operation, type, ids, data) {
    this.logToConsole('asyncRequest', [operation, type, ids, JSON.stringify(data)]);
    if (!ids) {
      ids = null;
    }
    if (!data) {
      data = null;
    }
    let payload = {
      msgType: TYPE_MODEL_REQ,
      resourceType: type,
      operation: operation,
      resourceIds: ids,
      data: this.transformRequest(data, type, operation)
    };
    this.respSemaphoreAcquire();
    return this.sendAndRegisterPromise(operation, type, payload);
  },

  /**
   * Sends a payload (JSON) via WebSocket, previously adding a randomly
   * generated UUID to it and registers a promise
   * (which can later be retrieved by the UUID).
   *
   * TODO: document type of "payload" (Message without uuid?)
   */
  sendAndRegisterPromise(operation, type, payload) {
    // Add UUID to payload so we can later connect the response with a promise
    // (the server will include this uuid in the response)
    let uuid = this.generateUuid();
    payload.uuid = uuid;
    let adapter = this;
    return new Ember.RSVP.Promise(function (resolve, reject) {
      let success = function (json) {
        Ember.run(null, resolve, json);
      };
      let error = function (json) {
        Ember.run(null, reject, json);
      };
      adapter.promises.set(uuid, {
        success: success,
        error: error,
        type: type,
        operation: operation
      });
      console.debug('registerPromise: ' + JSON.stringify(payload));
      adapter.send(payload);
    });
  },

  /** Generates a random uuid */
  generateUuid() {
    let date = new Date().getTime();
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g,
      function (character) {
        let random = (date + Math.random() * 16) % 16 | 0;
        date = Math.floor(date / 16);
        return (character === 'x' ? random : (random & 0x7 | 0x8)).toString(16);
      });
  },

  /**
   * Used to transform some types of requests, because they carry different
   * information than Ember assumes.
   */
  transformRequest(json, type, operation) {
    switch (operation) {
      case OP_CREATE_RECORD:
        return json[type] || json[type.camelize()];

        // case OP_QUERY:
        // case OP_QUERY_RECORD:
      default:
        return json;
    }
  },

  /**
   * Transform response received from WebSocket to the format expected
   * by Ember.
   */
  transformResponse(json, type, operation) {
    if (FETCH_MODEL_OPERATIONS.has(operation)) {
      let result = {};
      result[type] = json;
      return result;
    } else {
      return json;
    }
  },

  /** WebSocket onopen callback */
  open(event) {
    const onOpen = this.get('onOpenCallback');
    if (onOpen) {
      onOpen(event);
    }
    // Flush messages waiting for connection open
    this.debounce(this.flushMessageBuffer, FLUSH_TIMEOUT)();
  },

  /** Used to send a message (JSON) through WebSocket.  */
  send(payload) {
    this.messageBuffer.push(payload);
    this.debounce(this.flushMessageBuffer, FLUSH_TIMEOUT)();
  },

  /** Flushes a whole batch of messages that has been accumulated. Makes sure
   * that at least FLUSH_TIMEOUT has passed since last batch update.
   * If the WS is not established yet, it will wait in the buffer until
   * the connection is on. */
  flushMessageBuffer() {
    console.debug('flush' + this);
    const adapter = this;
    if (adapter.messageBuffer.length > 0 && this.socket) {
      if (this.socket.readyState === 1) {
        let batch = { batch: [] };
        adapter.messageBuffer.forEach(function (payload) {
          batch.batch.push(payload);
        });
        adapter.messageBuffer = [];
        adapter.socket.send(JSON.stringify(batch));

        // readyState > 1 means that WS is closing/closed, so we reject promises
        // to avoid indefinitely wait for WS to be opened again (maybe TODO)
      }
      if (this.socket.readyState > 1) {
        adapter.messageBuffer.forEach((message) => {
          const promise_spec = adapter.promises.get(message.uuid);
          promise_spec.error({ message: 'Cannot send message - WebSocket closed' });
        });
      }
    }
  },

  /** Applies a function after a timeout, but the timer is
   * reset every time the function is called within it's countdown. */
  debounce(func, wait, immediate) {
    var timeout;
    return () => {
      var context = this,
        args = arguments;
      var later = function () {
        timeout = null;
        if (!immediate) {
          func.apply(context, args);
        }
      };
      var callNow = immediate && !timeout;
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
      if (callNow) {
        func.apply(context, args);
      }
    };
  },

  /** WebSocket onmessage callback, resolves promises with received replies. */
  receive(event) {
    let json = JSON.parse(event.data);
    if (Array.isArray(json.batch)) {
      // as the for..of loop is currently tanspiled with Babel
      // this can slightly increase performance
      // and facilitates debugging
      if (json.batch.length === 1) {
        this.processMessage(json.batch[0]);
      } else {
        for (let message of json.batch) {
          this.processMessage(message);
        }
      }
    } else {
      console.warn(
        'A json.batch message was dropped because is not an Array, see debug logs for details'
      );
      console.debug('Warning: dropping message: ' + JSON.stringify(json));
    }
  },

  processMessageLater(message) {
    console.debug(`A message (uuid=${message.uuid}) will be processed later`);
    let waitingMessages = this.get('waitingMessages');
    waitingMessages.pushObject(message);
  },

  processQueuedMessages() {
    let waitingMessages = this.get('waitingMessages');
    /* jshint loopfunc: true */
    while (waitingMessages.get('length') > 0) {
      let message = waitingMessages.shift();
      setTimeout(() => this.processMessage(message), 0);
    }
  },

  waitingMessagesChanged: Ember.observer('isRespSemaphoreAcquired',
    'waitingMessages.length',
    function () {
      let {
        isRespSemaphoreAcquired,
        waitingMessages
      } = this.getProperties(
        'isRespSemaphoreAcquired',
        'waitingMessages'
      );

      if (!isRespSemaphoreAcquired && waitingMessages.get('length') > 0) {
        console.debug('respSemaphore has been released - processing waitingMessages');
        this.processQueuedMessages();
      }
    }),

  // TODO: document message object: data, uuid, result
  processMessage(message) {
    let adapter = this;
    let store = this.get('store');
    let promise;
    let {
      msgType,
      uuid,
      result,
      data,
      resourceType
    } = message;

    console.debug('Processing message: ' + JSON.stringify(message));

    switch (msgType) {
      case TYPE_MODEL_RESP:
        try {
          // Received a response to data fetch
          promise = adapter.promises.get(uuid);
          if (result === RESULT_OK) {
            let transformed_data = this.transformResponse(
              data,
              promise.type,
              promise.operation
            );
            console.debug(
              `FETCH_RESP success, (uuid=${message.uuid}): ${JSON.stringify(transformed_data)}`
            );
            promise.success(transformed_data);
          } else if (result === RESULT_ERROR) {
            console.debug(`FETCH_RESP error, (uuid=${uuid}): ${JSON.stringify(data)}`);
            promise.error(data);
          } else {
            console.warn(
              `Received model response (uuid=${uuid}) with unknown result: ${result}`
            );
            promise.error(data);
          }
        } finally {
          this.respSemaphoreRelease();
        }

        break;

      case TYPE_RPC_RESP:
        try {
          // Received a response to RPC call
          promise = adapter.promises.get(uuid);
          if (result === RESULT_OK) {
            console.debug(`RPC_RESP success, (uuid=${uuid}): ${JSON.stringify(data)}`);
            promise.success(data);
          } else if (result === RESULT_ERROR) {
            console.debug(`RPC_RESP error, (uuid=${uuid}): ${JSON.stringify(data)}`);
            promise.error(data);
          } else {
            console.warn(
              `Received RPC response (uuid=${uuid}) with unknown result: ${result}`);
            promise.error(data);
          }
        } finally {
          this.respSemaphoreRelease();
        }

        break;

      case TYPE_MODEL_CRT_PUSH:
      case TYPE_MODEL_UPT_PUSH:
        // Received a push message that something was created
        if (this.get('isRespSemaphoreAcquired')) {
          this.processMessageLater(message);
        } else {
          console.debug(msgType + ': ' + JSON.stringify(message));
          let payload = {};
          payload[resourceType] = data;
          store.pushPayload(payload);
        }
        break;

      case TYPE_MODEL_DLT_PUSH:
        // Received a push message that something was deleted
        // data field contains a list of ids to delete
        if (this.get('isRespSemaphoreAcquired')) {
          this.processMessageLater(message);
        } else {
          data.forEach(function (id) {
            store.findRecord(resourceType, id).then(
              function (record) {
                store.unloadRecord(record);
              });
          });
        }
        break;

      case TYPE_PUSH_MESSAGE:
        this.get('serverMessagesHandler').triggerEvent(
          data.operation,
          data.arguments
        );
        break;

      default:
        console.warn(`Server message with unknown type received: ${msgType}`);
        break;
    }

    if (uuid) {
      adapter.promises.delete(uuid);
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
  }
});
