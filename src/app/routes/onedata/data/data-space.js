/**
 * Load model for space - to be able to browse it's root dir.
 *
 * @module routes/data-space
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

const {
  inject: { service },
  computed: { reads },
  get,
} = Ember;

export default Ember.Route.extend(RouteRejectHandler, {
  session: service(),
  remoteOneprovider: service(),
  
  providerId: reads('session.sessionDetails.providerId'),
  
  // TODO: rejection of model causes a file tree component to be broken
  // url resolves correct - this is a common problem with routing in data spaces
  fallbackRoute: 'onedata.data.index',

  model(params) {
    return this.handleReject(
      this.store.findRecord('space', params.data_space_id)
    );
  },
  
  afterModel(model) {
    this._super(...arguments);
    const {
      providerId,
      remoteOneprovider,
    } = this.getProperties('providerId', 'remoteOneprovider');
    return remoteOneprovider.resolveOrRedirectOneprovider({
      space: model,
      currentProviderId: providerId,
      type: 'data',
      resourceId: get(model, 'id'),
      loadingArea: 'content-with-secondary-top',
    });
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
