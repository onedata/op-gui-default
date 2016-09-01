import Ember from 'ember';

/**
 * Created just to try to redirect to default group show option using controller.
 * See controllers/groups/show/index for details.
 * @module routes/groups/show/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  notify: Ember.inject.service(),

  model() {
    return this.modelFor('onedata.groups.show');
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.goToDefaultOption();
  },

});
