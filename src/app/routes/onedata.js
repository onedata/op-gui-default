import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

const {
  inject
} = Ember;

export default Ember.Route.extend(AuthenticatedRouteMixin, {
  session: inject.service(),

  beforeModel() {
    // Added to remember about invoking super for AuthenticatedRouteMixin
    this._super(...arguments);
  },
  
  model() {
    return this.get('session.user');
  },
});
