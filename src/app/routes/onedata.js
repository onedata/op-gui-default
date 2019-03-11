import Ember from 'ember';

const {
  inject: { service },
  get,
} = Ember;

export default Ember.Route.extend({
  session: service(),
  commonLoader: service(),
  adapter: function () {
    return this.get('store').adapterFor('application');
  }.property(),

  beforeModel() {
    this._super(...arguments);
    const commonLoader = this.get('commonLoader');
    if (get(commonLoader, 'type') === 'login') {
      commonLoader.reset();
    }
    return this.get('adapter').clearWebsocket()
      .then(() => this.get('session').initSession(false));
  },

  model() {
    sessionStorage.setItem('redirectFromOnezone', 'false');
    return this.get('session.user');
  },
});
