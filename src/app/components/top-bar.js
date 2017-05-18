import Ember from 'ember';

const {
  computed,
  inject
} = Ember;

/**
 * Just a top bar - container for toolbars and other stuff.
 * @module components/top-bar
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  /** Session is needed for account dropdown */
  session: inject.service(),

  aboutOpened: false,

  userName: computed.alias('session.user.name'),
  
  actions: {
    showAbout() {
      this.set('aboutOpened', true);
    },
  },
});
