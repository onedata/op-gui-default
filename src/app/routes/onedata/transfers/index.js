import Ember from 'ember';

/**
 * Created just to try to redirect to default transfers per space using controller.
 * See controllers/transfers/index for details.
 * @module routes/transfers/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  model() {
    return this.modelFor('onedata.transfers');
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.goToDefaultTransfersForSpace(model);
  },
});
