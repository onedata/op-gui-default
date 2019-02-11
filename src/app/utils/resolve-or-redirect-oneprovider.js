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
import checkImg from 'op-worker-gui/utils/check-img';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

const {
  get,
  RSVP: { Promise },
} = Ember;

export default function resolveOrRedirectOneprovider({ space, currentProviderId, type, resourceId, commonLoader, loadingArea }) {
  if (commonLoader) {
    commonLoader.setProperties({
      isLoading: true,
      solidBackground: true,
      message: 'Checking supporting Oneprovider...',
      area: 'content-with-secondary-top',
      type: 'resolveOneprovider'
    }); 
  }
  return get(space, 'providerList').then(providerList => {
    const supportingProviderIds = get(providerList, 'list');
    if (supportingProviderIds.indexOf(currentProviderId) !== -1) {
      return space;
    } else {
      return get(providerList, 'queryList')
        .then(providers => {
          const onlineProvider = providers.filter(p => get(p, 'status') === 'online')[0];
          if (onlineProvider) {
            return checkImg(`https://${get(onlineProvider, 'domain')}/assets/images/dir.png`)
              .then(isAvailable => {
                if (isAvailable) {
                  return new Promise(() => {
                    window.location =
                      `${location.origin}/op/${get(onlineProvider, 'cluster')}/i#/onedata/${type}/${resourceId}`;
                  });
                } else {
                  // FIXME: show better error
                  throw new Error(
                    'The target Oneprovider\'s domain cannot be reached from this web browser'
                  );
                }
              });
          } else {
            // FIXME: show better error
            throw new Error('None of the supporting Oneproviders is available');
          }
        });
    }
  })
  .finally(() => {
    if (commonLoader && loadingArea && get(commonLoader, 'type') === 'resolveOneprovider') {
      safeExec(commonLoader, 'setProperties', {
        isLoading: false,
        solidBackground: undefined,
        message: null,
        area: null,
        type: null,
      });
    }
  });
}