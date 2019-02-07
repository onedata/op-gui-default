/**
 * Redirects to a default space, as the empty view with selected space
 * doesn't make sense.
 *
 * @module routes/data/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

import getDefaultSpace from 'op-worker-gui/utils/get-default-space';

const {
  inject: { service },
  computed,
} = Ember;

export default Ember.Route.extend({
  session: service(),
  
  providerId: computed.reads('session.sessionDetails.providerId'),
  
  model() {
    return this.modelFor('onedata.data');
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.onDataSpacesChange();
    return Ember.run.scheduleOnce('afterRender', () => {
      return getDefaultSpace(model, this.get('providerId'))
        .then(space => this.send('goToDataSpace', space));
    });
  }
});
