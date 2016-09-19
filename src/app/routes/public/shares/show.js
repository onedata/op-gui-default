import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

/**
 * A single Share route.
 * @module routes/public/shares/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(RouteRejectHandler, {
  model(params) {
    return this.handleReject(this.store.find('share-public', params.share_id));
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.modelChanged();
  }
});
