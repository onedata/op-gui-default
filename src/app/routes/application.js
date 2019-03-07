/**
 * A main route, setting up whole application.
 * @module routes/application
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import { WebFont } from 'webfontloader';

const {
  inject: {
    service
  },
  RSVP: { Promise },
} = Ember;

export default Ember.Route.extend({
  session: service(),
  messageBox: service(),

  actions: {
    transitionTo() {
      this.transitionTo(...arguments);
    }
  },

  model() {
    return new Promise((resolve, reject) => {
      WebFont.on('active', resolve, true);
      WebFont.on('inactive', reject);
    });
  },
});
