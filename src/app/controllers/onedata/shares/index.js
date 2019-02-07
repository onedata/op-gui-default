/**
 * Redirect to default share if available.
 * @module onedata/controllers/shares/index
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

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

  onModelChange: Ember.observer('model.[]', 'model.@each.id', function() {
    // TODO: the observer works even if we are not on this route
    if (this.get('isActive')) {
      this.goToDefault();
    }
  }),
});

function resolveSupportedShare(shares, i, currentProviderId) {
  const selectedShare = shares[i];
  if (selectedShare) {
    return get(selectedShare, 'dataSpace')
      .then(space => {
        return get(space, 'providerList')
          .then(providerList => {
            const supportingProviderIds = get(providerList, 'list');
            if (supportingProviderIds.indexOf(currentProviderId) !== -1) {
              return selectedShare;
            } else {
              return resolveSupportedShare(shares, i + 1, currentProviderId);
            }
          }); 
      });
  } else {
    return null;
  }
}
