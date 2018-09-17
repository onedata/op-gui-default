/**
 * Parent of routes that redirect to external Onezone app views
 *
 * @module routes/onezone
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Route,
  inject: { service },
} = Ember;

export default Route.extend({
  session: service(),
  notify: service(),
  i18n: service(),

  beforeModel(transition) {
    if (transition.queryParams.back_forward ||
      performance && performance.navigation.type === performance.navigation.TYPE_BACK_FORWARD
    ) {
      console.debug(
        'route:onezone: detected back/forward - redirecting to onedata route'
      );
      delete transition.queryParams.back_forward;
      const hashBeforeRedirect = sessionStorage.getItem('hash-before-redirect');
      sessionStorage.clear('hash-before-redirect');
      transition.abort();
      window.location.replace(hashBeforeRedirect || '#/');
    } else {
      const currentHash = window.location.hash;
      if (!/\/onezone/.test(currentHash)) {
        sessionStorage.setItem('hash-before-redirect', currentHash);
      } else {
        sessionStorage.clear('hash-before-redirect');
      }
    }
  },

  model(params, transition) {
    const onezoneUrl = this.get('session.sessionDetails.manageProvidersURL');
    if (!onezoneUrl) {
      this.get('notify').error(this.get('i18n').t('onezone.cannotResolveUrl'));
      transition.abort();
    }
    return {
      onezoneUrl,
    };
  },
});
