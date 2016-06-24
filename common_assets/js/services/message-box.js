import Ember from 'ember';

const DEFAULT_TYPE = 'info';
const DEFAULT_ALLOW_CLOSE = true;

/**
 * A service for global access to message-box component (which should be placed
 * in application template).
 *
 * It allows to display a simple modal with an information and optional close button.
 * Modals without close button cannot be closed by user (without JS manipulations).
 * Therefore they could be used only when application should be locked or there was
 * a fatal error.
 *
 * The main method of the service is "open" - see its doc for details.
 * Its properties can be also manipulated withou open/close - see object properties.
 *
 * @module services/message-box
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  /** Is modal currently visible (boolean) */
  isOpened: false,

  /** Title of modal (string) */
  title: null,

  /** Message in modal body (string) */
  message: null,

  /** If true, show a close button (bool) */
  allowClose: true,

  /**
   * String, indicates type of message box
   * One of: info, warning, error
   */
  type: DEFAULT_TYPE,

  /**
   * Opens the message box modal.
   *
   * @param {Object} properties
   * @param {string} [title=null] a title of modal
   * @param {string} [message=null] a message in body of modal
   * @param {boolean} [allowClose=true] if true, allow to close the modal with "OK" button
   * @param {string} [type=info] style of information - one of: info, warning, error, loading
   */
  open(properties) {
    this.setProperties({
      isOpened: true,
      title: properties.title || null,
      message: properties.message || null,
      allowClose: ('allowClose' in properties) ? properties.allowClose : DEFAULT_ALLOW_CLOSE,
      type: properties.type || DEFAULT_TYPE
    });
  },

  _resetProperties() {
    this.setProperties({
      isOpened: false,
      title: null,
      message: null,
      allowClose: DEFAULT_ALLOW_CLOSE,
      type: DEFAULT_TYPE,
    });
  },

 /** Close the modal and reset data in it */
  close() {
    this.set('isOpened', false);
  },
});
