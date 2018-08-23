/**
 * Redirect to spaces management in Onezone
 *
 * @module routes/onezone/providers
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Route,
} = Ember;

export default Route.extend({
  /**
   * Redirect to spaces page of Onezone
   * @param {object} model contains onezoneUrl: string
   * @param {Ember.Transition} transition 
   */
  redirect(model) {
    window.location = `${model.onezoneUrl}/#/onedata/spaces`;
  },
});
