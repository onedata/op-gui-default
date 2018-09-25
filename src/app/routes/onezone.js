/**
 * Parent of routes that redirect to external Onezone app views
 *
 * @module routes/onezone
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import RedirectRoute from 'ember-cli-onedata-common/mixins/routes/redirect';

const {
  Route,
  inject: { service },
} = Ember;

export default Route.extend(RedirectRoute, {
  session: service(),
  notify: service(),
  i18n: service(),

  /** 
   * @override
   */
  checkComeFromOtherRoute(currentHash) {
    return !/\/onezone/.test(currentHash);
  },

  beforeModel() {
    return this._super(...arguments);
  },

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
