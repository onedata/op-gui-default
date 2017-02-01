/**
 * A main route, setting up whole application.
 * @module routes/spaces
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

const {
  inject: {
    service
  },
  on
} = Ember;

export default Ember.Route.extend(ApplicationRouteMixin, {
  session: service(),

  actions: {
    goToItem(name) {
      this.transitionTo(`onedata.${name}.index`);
    },

    transitionTo() {
      this.transitionTo(...arguments);
    }
  },

  initSession: on('init', function() {
    let {
      session
    } = this.getProperties('session');

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

    return sessionInitialization;
  }),
});
