import Ember from 'ember';

/**
 * Returns a property path for eventName in ``server-messages-handler`` service
 */
function handlersProperty(eventName) {
  return `eventHandlers.${eventName}`;
}

/**
 * A global place where handlers for named events can be registered, and then invoked.
 * It is written for server push messages handling - each server message has
 * a ``operation`` field which is an event name. A ``ws-adapter`` invokes this
 * service when the server message is received.
 *
 * Use a ``onMessage(eventName, handler)`` method to register handlers for
 * pushed messages. Each message type could have multiple handlers - ``onMessage``
 * adds a new handler to existing list.
 *
 * All handlers for specific eventName can be deregisterd using ``offMessage``.
 *
 * @param
 * @returns
 */
export default Ember.Service.extend({
  /**
   * Stores a map of
   * @type {Ember.Object}
   */
  eventHandlers: null,

  init() {
    this._super();
    this.set('eventHandlers', Ember.Object.create({}));
  },

  /**
   * Adds a handler function to list of handlers for event.
   * @param {string} eventName
   * @param {function} handler
   */
  onMessage(eventName, handler) {
    const handlersPath = handlersProperty(eventName);
    if (eventName) {
      if (this.get(handlersPath) == null) {
        this.offMessage(eventName);
      }
      this.get(handlersPath).pushObject(handler);
    }
  },

  /**
   * Clears all handlers for event by creating empty handlers array.
   * @param {string} eventName
   */
  offMessage(eventName) {
    if (eventName) {
      this.set(handlersProperty(eventName), Ember.A());
    }
  },

  /**
   * Launch an event which will invoke all registered handlers for ``eventName``
   * @param {string} eventName name of event, same as an ``operation`` field
   *  in server message
   * @param {object} data an argument passed to handler function
   */
  triggerEvent(eventName, data) {
    const handlers = this.get(handlersProperty(eventName));
    if (handlers) {
      handlers.forEach((h) => {
        h(data);
      });
    }
  }
});
