import Ember from 'ember';

/**
 * A container for a content, that can be set to display:
 * - **blocking message** - only message is visible, content is hidden
 * - **non-blocking message** - a message is displayed on top of content (both are visible)
 * - **no message** - only content is displayed
 *
 * @module components/status-panel
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  /**
   * Indicate if status should be only content of this widget.
   * If true, only status and message will be rendered.
   * If false, it will render status and message panel as well as yielded content.
   * @type Boolean
   */
  blocking: null,

  /**
   * One of: "info", "warning", "error", "loading" or null
   * If null - the status will be not rendered at all, only yielded content.
   * @type String
   */
  type: null,

  /**
   * A text of message to be displayed with status indicator.
   * @type String
   */
  message: null,

  /**
   * An optional bold part of a message.
   * @type String
   */
  messagePrefix: null,

  alertPanelType: Ember.computed('type', function() {
    const type = this.get('type');

    switch (type) {
      case 'error':
        return 'danger';
      default:
        return type;
    }
  }),

  iconClass: Ember.computed('type', function() {
    return this.get('type') + ' oneicon oneicon-sign-' + this.get('type');
  }),
});
