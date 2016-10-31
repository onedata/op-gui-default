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

  handle(errorEvent) {
    let consoleFunName;
    let notifyFunName;
    switch (errorEvent.severity) {
      case 'warning':
        consoleFunName = 'warn';
        notifyFunName = 'warning';
        break;
      case 'error':
      case 'critical':
        consoleFunName = 'error';
        notifyFunName = 'error';
        break;
    
      default:
        consoleFunName = 'error';
        notifyFunName = 'error';
     }
    console[consoleFunName]('[ERROR-NOTIFIER] ' + errorEvent.message);
    this.get('notify')[notifyFunName](errorEvent.message);
  }
});
