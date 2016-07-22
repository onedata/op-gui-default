import Ember from 'ember';
import RouteRejectHandler from '../../mixins/route-reject-handler';

/**
 * Load model for space - to be able to browse it's root dir.
 *
 * @module routes/data
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(RouteRejectHandler, {
  // TODO: rejection of model causes a file tree component to be broken
  // url resolves correct - this is a common problem with routing in data spaces
  fallbackRoute: 'data.index',

  model(params) {
    return this.handleReject(this.store.findRecord('data-space', params.data_space_id));
  },

  activate() {
    this.controllerFor(this.routeName).setSelectedSpace();
  },

  renderTemplate() {
    this.render({outlet: 'data-space'});
  },

  actions: {
    openDirInBrowser(fileId) {
      this.transitionTo('data.data-space.dir', fileId);
    },

    goToDataSpace(spaceId) {
      return spaceId !== this.controllerFor(this.routeName).get('model.id');
    }
  }
});
