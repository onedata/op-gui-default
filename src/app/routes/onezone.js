/**
 * Parent of routes that redirect to external Onezone app views
 *
 * @module routes/onezone
 * @author Jakub Liput
 * @copyright (C) 2017-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Route,
  inject: { service },
  RSVP: { resolve },
  get,
  computed,
} = Ember;

export default Route.extend({
  session: service(),
  notify: service(),
  i18n: service(),

  adapter: computed(function adapter() {
    return this.get('store').adapterFor('application');
  }),

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
