import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

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
  remoteOneprovider: service(),
  fileSystemTree: service(),
  secondaryMenu: service(),

  providerId: reads('session.sessionDetails.providerId'),

  fallbackRoute: 'onedata.transfers.index',

  model(params) {
    return this.handleReject(this.store.find('space', params.space_id));
  },

  afterModel(model, transition) {
    this.handleAfterModelErrors(model);
    this._super(...arguments);
    const {
      providerId,
      remoteOneprovider,
    } = this.getProperties('providerId', 'remoteOneprovider');
    return remoteOneprovider.resolveOrRedirectOneprovider({
      space: model,
      currentProviderId: providerId,
      type: 'transfers',
      resourceId: get(model, 'id'),
      loadingArea: 'content',
      transition,
    }).then(space => {
      if (space) {
        return space;
      } else {
        transition.abort();
        const m = /.*\/onedata\/transfers\/((.*)\?.*|(.*))/.exec(location.hash);
        const spaceId = m[2] || m[1];
        transition.finally(() => {
          if (spaceId && spaceId !== get(model, 'id')) {
            this.transitionTo('onedata.transfers.show', spaceId);
          } else {
            this.transitionTo('onedata.transfers.index');
          }
        });
      }
    });
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
