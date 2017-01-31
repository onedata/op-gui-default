import Ember from 'ember';

/**
 * Created just to try to redirect to default space show option using controller.
 * See controllers/spaces/show/index for details.
 * @module routes/spaces/show/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  model() {
    return this.modelFor('onedata.spaces.show');
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.goToDefaultOption();
  },
});
