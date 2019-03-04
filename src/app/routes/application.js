/**
 * A main route, setting up whole application.
 * @module routes/application
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';
import { WebFont } from 'webfontloader';

const {
  inject: {
    service
  },
  RSVP: { Promise },
} = Ember;

export default Ember.Route.extend(ApplicationRouteMixin, {
  session: service(),
  messageBox: service(),

  actions: {
    transitionTo() {
      this.transitionTo(...arguments);
    }
  },

  init() {
    this._super(...arguments);
    this.initSession();
  },

  model() {
    return new Promise((resolve, reject) => {
      WebFont.on('active', resolve, true);
      WebFont.on('inactive', reject);
    });
  },

  initSession() {
    return this.get('session').initSession()
      .then(() => {
        console.debug('route:application: initSession resolved');
      })
      .catch(error => {
        console.debug('route:application: initSession rejected');
        // TODO: messageBox doesn't work here because of loading route
        this.get('messageBox').open({
          type: 'error',
          allowClose: false,
          title: 'Session initialization error',
          message: 'Fatal error: session cannot be initialized'
        });
        throw error;
      });
  },
});
