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

export default Ember.Route.extend({
  model() {
    return this.modelFor('onedata.data');
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.onDataSpacesChange();
    this.send('goToDataSpace', getDefaultSpace(model));
  }
});
