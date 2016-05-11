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

  beforeModel() {

  },

  model() {
    return this.modelFor('groups.show');
  },

  activate() {
    this.controllerFor(this.routeName).goToDefaultOption();
  },
});
