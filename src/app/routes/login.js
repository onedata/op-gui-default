import Ember from 'ember';

import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

const SECONDS_TO_REDIRECT = 5;

export default Ember.Route.extend(UnauthenticatedRouteMixin, {
  loginRedirect: Ember.inject.service(),

  setupController(controller/*, model*/) {
    controller.set('secondsToRedirect', SECONDS_TO_REDIRECT);
    this.get('loginRedirect').set('secondsToRedirect', SECONDS_TO_REDIRECT);
    this.get('loginRedirect').startTimeout();
  },
});
