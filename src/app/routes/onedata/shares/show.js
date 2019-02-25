import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

const {
  get,
  inject: { service },
  computed: { reads },
} = Ember;

export default Ember.Route.extend(RouteRejectHandler, {
  session: service(),
  remoteOneprovider: service(),
  fileSystemTree: service(),
  
  providerId: reads('session.sessionDetails.providerId'),
  
  fallbackRoute: 'onedata.shares',

  model(params) {
    return this.handleReject(
      this.store.find('share', params.share_id)
        .then(share => share.get('handle').then(() => share))
    );
  },
  
  afterModel(model, transition) {
    this._super(...arguments);
    const {
      providerId,
      remoteOneprovider,
    } = this.getProperties('providerId', 'remoteOneprovider');
    return get(model, 'dataSpace')
      .then(space =>
        remoteOneprovider.resolveOrRedirectOneprovider({
          space,
          currentProviderId: providerId,
          type: 'shares',
          resourceId: get(model, 'id'),
          loadingArea: 'content',
        })
      )
      .then(space => {
        if (!space) {
          transition.then(() => {
            this.get('fileSystemTree').backToPrevSpace();
          });
        } else {
          return model;
        }
      });
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.containerDirChanged();
  }
});
