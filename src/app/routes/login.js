import Ember from 'ember';

import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

const {
  inject: {
    service
  },
  Route,
  computed: {
    alias
  },
  observer
} = Ember;

export default Route.extend(UnauthenticatedRouteMixin, {
  session: service(),
  eventsBus: service(),

  isSessionValid: alias('session.sessionValid'),

  redirectToSystemLogin() {
    window.location = "/login.html";
  },

  beforeModel() {
    this.redirectIfSessionInitialized();
  },

  redirectIfSessionInitialized: observer('isSessionValid', function() {
    let isSessionValid = this.get('isSessionValid');
    if (isSessionValid === false) {
      this.redirectToSystemLogin();
    }
  }),
});
