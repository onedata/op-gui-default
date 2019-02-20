/**
 * Redirect to clusters management in Onezone
 *
 * @module routes/onezone/clusters
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Route,
} = Ember;

export default Route.extend({
  /**
   * Redirect to clusters page of Onezone
   * @param {object} model contains onezoneUrl: string
   * @param {Ember.Transition} transition 
   */
  redirect(model) {
    window.location = `${model.onezoneUrl}/#/onedata/clusters`;
  },
});
