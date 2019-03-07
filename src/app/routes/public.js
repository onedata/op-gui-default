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
    this.get('adapter').clearWebsocket();
    return this.get('session').initSession(true);
  },

  model() {
    sessionStorage.setItem('redirectFromOnezone', 'false');
  },
});
