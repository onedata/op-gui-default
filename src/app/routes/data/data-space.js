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
  // FIXME: rejection of model causes a file tree component to be broken
  // (url resolves correct)
  fallbackRoute: 'data.index',

  model(params) {
    return this.handleReject(this.store.findRecord('data-space', params.data_space_id));
  },

  afterModel(dataSpace) {
    if (dataSpace) {
      Ember.run.scheduleOnce('afterRender', this, function() {
        console.debug('selected data-space: ' + dataSpace.get('id'));
        // TODO: this should use data-spaces-select service or something...
      });
    }
  },

  activate() {
    this.controllerFor(this.routeName).onDataSpaceChange();
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
