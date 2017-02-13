import Ember from 'ember';
import ModalMixin from 'ember-cli-onedata-common/mixins/components/modal';
import layout from 'ember-cli-onedata-common/templates/components/modals/elements/alert-panel';

/**
 * A small Bootstrap alert panel ("alert" class) which displays response status
 * of modal actions. Properties marked as ``@abstract`` should be injected.
 * @module components/modals/elements/alert-panel
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(ModalMixin, {
  layout,

  /**
   * A ``<strong>`` part of displayed essage - can be a only message or a prefix for "message"
   * @abstract
   */
  messagePrefix: null,

  /**
   * A user-specified custom message
   * @abstract
   */
  message: null,

  /**
   * One of: success, danger to indicate type of message
   * Use "success" for request success and "danger" for failure
   * @abstract
   */
  messageType: null,

  /**
   * If true, it renders a close button which allows to dismiss the message.
   * @type boolean
   * @default true
   */
  closeButton: true,

  /** CSS class for Bootstrap alert panel which is displayed after request complete */
  alertClass: function() {
    return `alert-${this.get('messageType')}`;
  }.property('messageType'),

});
