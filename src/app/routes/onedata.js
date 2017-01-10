import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

const {
  inject
} = Ember;

export default Ember.Route.extend(AuthenticatedRouteMixin, {
  session: inject.service(),

  model() {
    debugger;
    return this.get('session.user');
  },
});
