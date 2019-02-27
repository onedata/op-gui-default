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
              const onlineProvider = providers.filter(p => get(p, 'online'))[0];
              if (onlineProvider) {
                return this.chooseOneprovider(space, providers)
                  .then(chosenOneprovider => {
                    if (chosenOneprovider) {
                      return checkImg(
                          `https://${get(chosenOneprovider, 'domain')}/favicon.ico`
                        )
                        .then(isAvailable => {
                          if (isAvailable) {
                            return new Promise(() => {
                              window.location =
                                `${location.origin}/op/${get(chosenOneprovider, 'cluster')}/i#/onedata/${type}/${resourceId}`;
                            });
                          } else {
                            // FIXME: handle error
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
        if (commonLoader && loadingArea && get(commonLoader, 'type') ===
          'resolveOneprovider') {
          safeExec(commonLoader, 'reset');
        }
      });
  },

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
