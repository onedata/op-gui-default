import Ember from 'ember';

/**
 * A convenient (but limited) API to handle backend error objects.
 * An alternative to use the 'notify' service.
 * @module services/error-notifier
 * @author Jakub Liput
 * @author Łukasz Opioła
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  notify: Ember.inject.service('notify'),
  i18n: Ember.inject.service('i18n'),

  handle(errorEvent) {
    if (errorEvent.severity === 'warning') {
      this.get('notify').warning(errorEvent.message);
      console.error('[ERROR-NOTIFIER] ' + errorEvent.message);
    } else if (errorEvent.severity === 'error') {
      this.get('notify').error(errorEvent.message, {
        closeAfter: null
      });
      window.alert('[ERROR-NOTIFIER] ' + errorEvent.message);
    } else if (errorEvent.severity === 'critical') {
      // TODO: there are info, success, warning, alert and error in notify
      this.get('notify').alert(errorEvent.message, {
        closeAfter: null
      });
      window.alert('[ERROR-NOTIFIER] CRITICAL: ' + errorEvent.message);
    }
  }
});
