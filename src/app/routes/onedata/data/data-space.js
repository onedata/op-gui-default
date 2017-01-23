import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

/**
 * Load model for space - to be able to browse it's root dir.
 *
 * @module routes/data
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(RouteRejectHandler, {
  // TODO: rejection of model causes a file tree component to be broken
  // url resolves correct - this is a common problem with routing in data spaces
  fallbackRoute: 'onedata.data.index',

  model(params) {
    return this.handleReject(
      this.store.findRecord('space', params.data_space_id)
    );
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.setSelectedSpace(model);
  },

  renderTemplate() {
    this.render({outlet: 'data-space-sidebar'});
    this.render('application-loading', {outlet: 'data-content-scroll'});
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
