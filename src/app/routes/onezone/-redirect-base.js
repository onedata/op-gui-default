/**
 * Redirect to some path in Onezone GUI
 *
 * @module routes/onezone/-redirect-base
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import onezoneUrl from 'op-worker-gui/utils/onezone-url';

const {
  Route,
} = Ember;

export default Route.extend({
  /**
   * @virtual
   * Name of Onezone sidebar resources, eg. spaces, groups
   */
  resourceType: undefined,

  /**
   * Redirect to page in Onezone
   * @param {Ember.Transition} transition 
   */
  redirect() {
    window.location = onezoneUrl(`onedata/${this.get('resourceType')}`);
  },
});
