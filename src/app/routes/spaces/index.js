import Ember from 'ember';

/**
 * Created just to try to redirect to default space using controller.
 * See controllers/spaces/index for details.
 * @module routes/spaces/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  model() {
    return this.modelFor('spaces');
  },

  activate() {
    this.controllerFor(this.routeName).goToDefaultSpace();
  },
});
