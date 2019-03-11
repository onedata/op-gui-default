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
  RSVP: { resolve },
  get,
} = Ember;

export default Route.extend(RedirectRoute, {
  session: service(),
  notify: service(),
  i18n: service(),
  adapter: function () {
    return this.get('store').adapterFor('application');
  }.property(),

  /** 
   * @override
   */
  checkComeFromOtherRoute(currentHash) {
    return !/\/onezone/.test(currentHash);
  },

  beforeModel(transition) {
    this._super(...arguments);
    if (!get(transition, 'isAborted')) {
      if (this.get('adapter.socket')) {
        return resolve();
      } else {
        return this.get('session').initSession(true);
      }
    }
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
