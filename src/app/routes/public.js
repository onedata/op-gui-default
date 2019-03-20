import Ember from 'ember';

const {
  inject: { service },
  get,
} = Ember;

export default Ember.Route.extend({
  session: service(),
  commonLoader: service(),
  websocketConnection: service(),

  beforeModel() {
    this._super(...arguments);
    const commonLoader = this.get('commonLoader');
    if (get(commonLoader, 'type') === 'login') {
      commonLoader.reset();
    }
    return this.get('websocketConnection').clearWebsocket()
      .then(() => this.get('session').initSession(true));
  },

  model() {
    sessionStorage.setItem('redirectFromOnezone', 'false');
  },
});
