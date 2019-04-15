/**
 * Utils for using other Oneproviders than this, eg. when we want to open
 * space unsupported on this Oneprovider.
 * 
 * @module services/remote-oneprovider
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import checkImg from 'op-worker-gui/utils/check-img';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

const {
  Service,
  inject: { service },
  get,
  RSVP: { Promise, reject, defer },
} = Ember;

export default Service.extend({
  commonLoader: service(),
  i18n: service(),
  commonModals: service(),

  /**
   * Check if given `space` is supported by current Oneprovider - if it is not,
   * then get list of online supporting providers and ask user if he want to
   * redirect to some other Oneprovider.
   */
  resolveOrRedirectOneprovider({
    space,
    currentProviderId,
    type,
    resourceId,
    loadingArea = 'content-with-secondary-top',
    transition,
  }) {
    const {
      commonLoader,
      i18n,
    } = this.getProperties('commonLoader', 'i18n');
    commonLoader.setProperties({
      isLoading: true,
      solidBackground: true,
      message: i18n.t('services.remoteOneprovider.checkSupportingOneprovider'),
      area: loadingArea,
      type: 'resolveOneprovider'
    });
    return get(space, 'providerList').then(providerList => {
        const supportingProviderIds = get(providerList, 'list');
        if (get(supportingProviderIds, 'length') === 0) {
          throw {
            isOnedataCustomError: true,
            type: 'no-support',
            space,
            transition,
          };
        } else if (supportingProviderIds.indexOf(currentProviderId) !== -1) {
          return space;
        } else {
          return get(providerList, 'queryList')
            .then(providers => {
              const onlineProvider = providers.isAny('online');
              if (onlineProvider) {
                return this.chooseOneprovider(space, providers)
                  .then(chosenOneprovider => {
                    if (chosenOneprovider) {
                      const chosenOneproviderOrigin =
                        `https://${get(chosenOneprovider, 'domain')}`;
                      return checkImg(`${chosenOneproviderOrigin}/favicon.ico`)
                        .then(isAvailable => {
                          // workaround for legacy Oneproviders
                          return isAvailable || checkImg(
                            `${chosenOneproviderOrigin}/robots.txt`
                          );
                        })
                        .then(isAvailable => {
                          if (isAvailable) {
                            return new Promise(() => {
                              const clusterId = get(chosenOneprovider,
                                'cluster');
                              window.location =
                                `/ozw/onezone/i#/provider-redirect/${clusterId}?space_id=${resourceId}&resource_type=${type}`;
                            });
                          } else {
                            throw {
                              isOnedataCustomError: true,
                              type: 'endpoint-error',
                              space,
                              provider: chosenOneprovider,
                              transition,
                            };
                          }
                        });
                    } else {
                      return null;
                    }
                  });
              } else {
                throw {
                  isOnedataCustomError: true,
                  type: 'all-supporting-oneproviders-offline',
                  space,
                  providers,
                  transition,
                };
              }
            });
        }
      })
      .finally(() => {
        if (commonLoader && loadingArea &&
          get(commonLoader, 'type') === 'resolveOneprovider') {
          safeExec(commonLoader, 'reset');
        }
      });
  },

  /**
   * Show modal with Oneprovider selection and resolve with this `provider`
   * record
   */
  chooseOneprovider(space, providers) {
    try {
      const deferredProviderChoice = defer();
      this.get('commonModals').openModal('providerRedirect', {
        space,
        providers,
        deferredProviderChoice,
      });
      return deferredProviderChoice.promise;
    } catch (error) {
      return reject(error);
    }
  }
});
