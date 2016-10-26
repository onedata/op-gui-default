import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

export default Ember.Route.extend(RouteRejectHandler, {
  model(params) {
    return this.handleReject(this.store.find('file-shared', params.shared_dir_id));
  },

  actions: {
    openDirInBrowser(file) {
      this.transitionTo('onedata.shares.show.dir', file);
    },
  }
});
