/**
 * Redirect to default share if available.
 * @module onedata/controllers/shares/index
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import resolveSupportedResource from 'op-worker-gui/utils/resolve-supported-resource';

const {
  computed: { reads },
  inject: { service },
  get,
} = Ember;

export default Ember.Controller.extend({
  session: service(),

  providerId: reads('session.sessionDetails.providerId'),

  goToDefault() {
    console.debug(`shares.index: Will try to go to first share`);
    let shares = this.get('model').filterBy('isDeleted', false);
    if (shares && shares.get('length') > 0) {
      return resolveSupportedShare(shares.sortBy('name'), 0, this.get('providerId'))
        .then(share => {
          if (share) {
            return this.transitionToRoute(
              'onedata.shares.show',
              share
            );
          }
        });
    }
  },

  onModelChange: Ember.observer('model.[]', 'model.@each.id', function () {
    // TODO: the observer works even if we are not on this route
    if (this.get('isActive')) {
      this.goToDefault();
    }
  }),
});

export function resolveSupportedShare(shares, i, currentProviderId) {
  return resolveSupportedResource(shares, i, currentProviderId, (share) =>
    get(share, 'dataSpace')
    .then(space => get(space, 'providerList'))
    .then(providerList => get(providerList, 'list'))
  );
}
