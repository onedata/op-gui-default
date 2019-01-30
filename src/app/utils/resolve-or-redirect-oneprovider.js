/**
 * Using space record check if it is supported by this Oneprovider instance,
 * if so - just resolve this space, otherwise - try to redirect to one
 * of supporting Oneproviders.
 * 
 * @module utils/resolve-or-redirect-oneprovider
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
 
const {
  get,
  RSVP: { Promise },
} = Ember;
 
export default function resolveOrRedirectOneprovider(space, currentProviderId, type, resourceId) {
  return get(space, 'providerList').then(providerList => {
    const supportingProviderIds = get(providerList, 'list');
    if (supportingProviderIds.indexOf(currentProviderId) !== -1) {
      return space;
    } else {
      return get(providerList, 'queryList')
        .then(providers => {
          const onlineProvider = providers.filter(p => get(p, 'status') === 'online')[0];
          if (onlineProvider) {
            return new Promise(() => {
              window.location =
                `${location.origin}/op/${get(onlineProvider, 'cluster')}/i/#/onedata/${type}/${resourceId}`;
            });
          } else {
            throw new Error('None of the supporting Oneproviders is available');
          }
        });
    }
  });
}