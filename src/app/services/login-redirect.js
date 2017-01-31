import Ember from 'ember';

const {
  inject,
  computed
} = Ember;

/**
 * Redirect to login HTTP method when session is not valid.
 * Used in ``login`` and ``application`` routes.
 * @module services/login-redirect
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  session: inject.service('session'),

  secondsToRedirect: null,

  isSessionValid: computed.alias('session.sessionValid'),

  init() {
    this._super(...arguments);
    this.clearTimeouts();
  },

  redirect() {
    window.location = "/login.html";
  },

  startTimeout() {
    if (!this.get('isSessionValid')) {
      const rid = setTimeout(
        this.redirect.bind(this),
        this.get('secondsToRedirect')*1000
      );
      this.get('timeoutIds').pushObject(rid);
    } else {
      console.warn('Tried to start login redirect timeout, but session is already valid');
    }
  },

  clearTimeouts() {
    const tids = this.get('timeoutIds');
    if (tids) {
      tids.forEach(tid => {
        window.clearTimeout(tid);
      });
    }
    
    this.set('timeoutIds', Ember.A());
  },

  /**
   * Should be invoked, when ``session.initSession`` promise settled
   */
  onSessionInitFinished() {
    if (this.get('isSessionValid')) {
      console.debug('service:login-redirect: valid session resolved');
      this.clearTimeouts();
    } else {
      console.debug('service:login-redirect: session invalid or rejected - redirecting to OZ');
      this.redirect();
    }
  },
});
