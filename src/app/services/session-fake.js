import Ember from 'ember';
import SessionService from 'ember-simple-auth/services/session';

export default SessionService.extend({
  initSession() {
    return new Ember.RSVP.Promise((resolve) => {
      resolve();
    });
  },

  tryToRestoreSession() {
    new Ember.RSVP.Promise((resolve) => {
      resolve();
    });
  },
});
