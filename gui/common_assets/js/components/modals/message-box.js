import Ember from 'ember';

/**
 * A global component for displaying a simple modal with information and
 * optional close button.
 * @module components/modals/message-box
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  messageBox: Ember.inject.service(),

  allowClose: Ember.computed.alias('messageBox.allowClose'),
  open: Ember.computed.alias('messageBox.isOpened'),
  title: Ember.computed.alias('messageBox.title'),
  message: Ember.computed.alias('messageBox.message'),

  /**
   * String, indicates type of message box
   * One of: info, warning, error
   */
  type: Ember.computed.alias('messageBox.type'),

  actions: {
    closed() {
      this.get('messageBox')._resetProperties();
    }
  }
});
