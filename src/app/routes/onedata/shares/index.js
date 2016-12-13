import Ember from 'ember';

/**
 * All shares for user. Use controller to go to "default" (first on list) share.
 * @module routes/onedata/shares/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  model() {
    return this.modelFor('onedata.shares');
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.goToDefault();
  },
});
