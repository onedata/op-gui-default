import Ember from 'ember';

const {
  Route,
  inject: { service },
  get,
} = Ember;

export default Route.extend({
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
      .then(() => this.get('session').initSession(false));
  },

  model() {
    sessionStorage.setItem('redirectFromOnezone', 'false');
    return this.get('session.user');
  },
});
