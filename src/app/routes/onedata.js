import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

const {
  inject: { service },
  get,
} = Ember;

export default Ember.Route.extend(AuthenticatedRouteMixin, {
  session: service(),
  commonLoader: service(),

  beforeModel() {
    // Added to remember about invoking super for AuthenticatedRouteMixin
    this._super(...arguments);
    const commonLoader = this.get('commonLoader');
    if (get(commonLoader, 'type') === 'login') {
      commonLoader.reset();
    }
  },
  
  model() {
    return this.get('session.user');
  },
});
