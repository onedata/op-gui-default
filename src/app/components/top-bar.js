import Ember from 'ember';

/**
 * Just a top bar - container for toolbars and other stuff.
 * @module components/top-bar
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  /** Session is needed for account dropdown */
  session: Ember.inject.service()
});
