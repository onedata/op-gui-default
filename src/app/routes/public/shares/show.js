import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

// FIXME: currently this is a copied code from onedata/shares/show

export default Ember.Route.extend(RouteRejectHandler, {
  model(params) {
    return this.handleReject(this.store.find('share-public', params.share_id));
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.modelChanged();
  }
});
