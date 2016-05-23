import Ember from 'ember';

/**
 * Single space Route - loads Space data before actions/resources for a single
 * space.
 * @module routes/spaces/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  model(params) {
    return this.store.find('space', params.space_id);
  },

  setupController(controller, model) {
    controller.set('model', model);
  },

  actions: {
    /**
      Capture goToSpace event, because if we are already there,
      we do not want to make more actions.
      @param {Space} space model of space we want to change to
      @returns {Boolean} False, if space we want to go is current space (or not specified)
        True, if the other space was requested - so we pass the action up
    */
    goToSpace(space) {
      return !space || space.get('id') !== this.controllerFor(this.routeName).get('model.id');
    },
  }
});
