import Ember from 'ember';

import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

/**
 * How many seconds wait for session to resolve
 * until the redirection will be done.
 */
const SECONDS_TO_REDIRECT = 60;

export default Ember.Route.extend(UnauthenticatedRouteMixin, {
  loginRedirect: Ember.inject.service(),

  setupController(controller, model) {
    this._super(controller, model);
    let loginRedirect = this.get('loginRedirect');
    controller.set('secondsToRedirect', SECONDS_TO_REDIRECT);
    loginRedirect.set('secondsToRedirect', SECONDS_TO_REDIRECT);
    loginRedirect.startTimeout();
  },
});
