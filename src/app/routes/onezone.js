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

  model(params, transition) {
    const onezoneUrl = this.get('session.sessionDetails.onezoneURL');
    if (!onezoneUrl) {
      this.get('notify').error(this.get('i18n').t('onezone.cannotResolveUrl'));
      transition.abort();
    }
    return {
      onezoneUrl,
    };
  },
});
