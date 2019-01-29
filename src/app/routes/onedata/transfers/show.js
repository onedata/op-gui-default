import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';
import resolveOrRedirectOneprovider from 'op-worker-gui/utils/resolve-or-redirect-oneprovider';

const {
  Route,
  run,
  get,
  inject: { service },
  computed: { reads },
} = Ember;

/**
 * Single transfers for space Route - loads Space data before actions/resources
 * for a single space to show its transfers view
 * @module routes/transfers/show
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Route.extend(RouteRejectHandler, {
  session: service(),
  
  providerId: reads('session.sessionDetails.providerId'),
  
  fallbackRoute: 'onedata.transfers.index',

  model(params) {
    return this.handleReject(this.store.find('space', params.space_id));
  },

  afterModel(model) {
    this.handleAfterModelErrors(model);
    this._super(...arguments);
    const providerId = this.get('providerId');
    return resolveOrRedirectOneprovider(model, providerId, 'transfers', get(model, 'id'));
  },

  resetController(controller) {
    controller.resetQueryParams();
  },

  actions: {
    /**
      Capture goToTransfersForSpace event, because if we are already there,
      we do not want to make more actions.
      @param {Space} space model of space we want to change to
      @returns {Boolean} False, if space we want to go is current space (or not specified)
        True, if the other space was requested - so we pass the action up
    */
    goToTransfersForSpace(space) {
      return !space || space.get('id') !== this.controller.get('model.id');
    },
    
    didTransition() {
      run.scheduleOnce('afterRender', () => {
        this.controller.changeMenuActiveItem(this.model);
      });
      return true;
    },
  }
});
