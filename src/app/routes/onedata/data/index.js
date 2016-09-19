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

export default Ember.Route.extend({
  model() {
    return this.modelFor('onedata.data');
  },

  activate() {
    this.controllerFor(this.routeName).onDataSpacesChange();
  }
});
