import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';
import resolveOrRedirectOneprovider from 'op-worker-gui/utils/resolve-or-redirect-oneprovider';

const {
  get,
  inject: { service },
  computed: { reads },
} = Ember;

export default Ember.Route.extend(RouteRejectHandler, {
  session: service(),
  commonLoader: service(),
  
  providerId: reads('session.sessionDetails.providerId'),
  
  fallbackRoute: 'onedata.shares',

  model(params) {
    return this.handleReject(
      this.store.find('share', params.share_id)
        .then(share => share.get('handle').then(() => share))
    );
  },
  
  afterModel(model) {
    this._super(...arguments);
    const {
      commonLoader,
      providerId,
    } = this.getProperties('commonLoader', 'providerId');
    return get(model, 'dataSpace')
      .then(space =>
        resolveOrRedirectOneprovider({
          space,
          currentProviderId: providerId,
          type: 'shares',
          resourceId: get(model, 'id'),
          commonLoader,
          loadingArea: 'content',
        })
      )
      .then(() => model);
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.containerDirChanged();
  }
});
