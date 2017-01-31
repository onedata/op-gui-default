import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

/**
 * Single group Route - loads Group data before actions/resources for a single
 * space.
 * @module routes/groups/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(RouteRejectHandler, {
  secondaryMenu: Ember.inject.service(),

  fallbackRoute: 'onedata.groups.index',

  model(params) {
    return this.handleReject(this.store.find('group', params.group_id));
  },

  afterModel(model) {
    this.handleAfterModelErrors(model);
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.changeMenuActiveItem(model);
  },

  actions: {
    /**
      Capture goToGroup event, because if we are already there,
      we do not want to make more actions.
      @param {Space} space model of space we want to change to
      @returns {Boolean} False, if space we want to go is current space (or not specified)
        True, if the other space was requested - so we pass the action up
    */
    goToGroup(group) {
      return !group || group.get('id') !== this.controller.get('model.id');
    }
  }
});
