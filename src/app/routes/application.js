/**
 * A main route, setting up whole application.
 * @module routes/spaces
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
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
    let p = this.get('session').initSession();

    p.then(
      () => {
        console.debug('initSession resolved');
        this.get('loginRedirect').clearTimeouts();
      },
      // TODO: translations
      () => {
        this.get('messageBox').open({
          type: 'error',
          allowClose: false,
          title: 'Session initialization error',
          message: 'Fatal error: session cannot be initialized'
        });
      }
    );

    return p;
  }),
});
