import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

export default Ember.Route.extend(RouteRejectHandler, {
  fallbackRoute: 'onedata.shares',

  model(params) {
    return this.handleReject(
      this.store.find('share', params.share_id)
        .then(share => share.get('handle').then(() => share))
    );
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.containerDirChanged();
  }
});
