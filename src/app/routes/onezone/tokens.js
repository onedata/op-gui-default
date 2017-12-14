/**
 * Redirect to client tokens management in Onezone
 *
 * @module routes/onezone/tokens
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Route,
} = Ember;

export default Route.extend({
  /**
   * Redirect to tokens page of Onezone
   * @param {object} model contains onezoneUrl: string
   * @param {Ember.Transition} transition 
   */
  redirect(model) {
    window.location = `${model.onezoneUrl}?expand_tokens=true`;
  },
});
