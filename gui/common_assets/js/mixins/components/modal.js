import Ember from 'ember';

/**
 * Common logic of modals components.
 * - support for alert-panel (messageType, messagePrefix)
 * @module mixins/components/modal
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  /**
   * @abstract
   * A prefix of i18n translation key to compute a translation for exact component type
   */
  i18nPrefixKey: null,

  /**
   * @abstract
   * One of: success, danger to indicate type of message
   * Use "success" for request success and "danger" for failure
   */
  messageType: null,

  /** A prefix of info message displayed as alert panel - e.g. "Error: <message>" */
  messagePrefix: function() {
    const i18nPrefix = this.get('i18nPrefixKey');
    switch (this.get('messageType')) {
      case 'danger':
        return this.get('i18n').t(i18nPrefix + '.error');
      case 'success':
        return this.get('i18n').t(i18nPrefix + '.success');
      default:
        return null;
    }
  }.property('messageType'),
});
