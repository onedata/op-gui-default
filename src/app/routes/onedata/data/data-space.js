import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

const {
  get,
  RSVP: { Promise, reject },
  inject: { service },
  computed: { reads },
} = Ember;

/**
 * Load model for space - to be able to browse it's root dir.
 *
 * @module routes/data
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(RouteRejectHandler, {
  session: service(),
  
  providerId: reads('session.sessionDetails.providerId'),
  
  // TODO: rejection of model causes a file tree component to be broken
  // url resolves correct - this is a common problem with routing in data spaces
  fallbackRoute: 'onedata.data.index',

  model(params) {
    const providerId = this.get('providerId');
    return this.handleReject(
      this.store.findRecord('space', params.data_space_id)
        .then(space => {
          return get(space, 'providerList').then(providerList => {
            const supportingProviderIds = get(providerList, 'list');
            if (supportingProviderIds.indexOf(providerId) !== -1) {
              return space;
            } else {
              return get(providerList, 'queryList')
                .then(providers => {
                  const onlineProvider = providers.filter(p => get(p, 'status') === 'online')[0];
                  if (onlineProvider) {
                    return new Promise(() => {
                      window.location =
                      `${location.origin}/op/${get(onlineProvider, 'cluster')}/i/#/onedata/data/${get(space, 'id')}`;
                    });
                  } else {
                    // FIXME: better graphics
                    reject('None of the supporting Oneproviders is available');
                  }
                });
            }
          });
        })
    );
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.setSelectedSpace(model);
  },

  renderTemplate() {
    this.render({ outlet: 'data-space-sidebar' });
    this.render('application-loading', { outlet: 'data-content-scroll' });
  },

  actions: {
    openDirInBrowser(file) {
      this.transitionTo('onedata.data.data-space.dir', file);
    },

    goToDataSpace(spaceId) {
      return spaceId !== this.controller.get('model.id');
    }
  }
});
