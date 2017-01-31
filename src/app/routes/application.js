/**
 * A main route, setting up whole application.
 * @module routes/spaces
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

export default Ember.Route.extend(ApplicationRouteMixin, {
  mainMenuService: Ember.inject.service('main-menu'),
  session: Ember.inject.service('session'),
  loginRedirect: Ember.inject.service(),

  actions: {
    goToItem(name) {
      this.transitionTo(`onedata.${name}.index`);
    },

    transitionTo() {
      this.transitionTo(...arguments);
    }
  },

  initSession: Ember.on('init', function() {
    let {
      session,
      loginRedirect
    } = this.getProperties('session', 'loginRedirect');

    let sessionInitialization = session.initSession();

    sessionInitialization.then(() => {
      console.debug('route:application: initSession resolved');
    });
    // TODO: translations
    sessionInitialization.catch(() => {
      console.debug('route:application: initSession rejected');
      this.get('messageBox').open({
        type: 'error',
        allowClose: false,
        title: 'Session initialization error',
        message: 'Fatal error: session cannot be initialized'
      });
    });

    sessionInitialization.finally(() => {
      loginRedirect.onSessionInitFinished();
    });

    return sessionInitialization;
  }),
});
