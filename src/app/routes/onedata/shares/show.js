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
          transition,
        })
      )
      .then(space => {
        if (space) {
          return model;
        } else {
          transition.abort();
          const m = /.*\/onedata\/shares\/(.*)\//.exec(location.hash);
          const shareId = m[1];
          transition.finally(() => {
            if (shareId && shareId !== get(model, 'id')) {
              this.transitionTo('onedata.shares.show', shareId);
            } else {
              this.transitionTo('onedata.shares.index');
            }
          });
        }
      });
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.containerDirChanged();
  }
});
