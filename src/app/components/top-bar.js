import Ember from 'ember';

const {
  computed,
  inject
} = Ember;

/**
 * Just a top bar - container for toolbars and other stuff.
 * @module components/top-bar
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  /** Session is needed for account dropdown */
  session: inject.service(),

  aboutOpened: false,

  userName: computed.alias('session.user.name'),

  onezoneUrl: computed.reads('session.sessionDetails.onezoneURL'),

  logoutUrl: computed('onezoneUrl', function logoutUrl() {
    return `${this.get('onezoneUrl')}/logout`;
  }),

  manageAccountUrl: computed('session.sessionDetails.onezoneURL', function manageAccountLink() {
    const onezoneUrl = this.get('session.sessionDetails.onezoneURL');
    return `${onezoneUrl}/#/onedata/users`;
  }),

  actions: {
    showAbout() {
      this.set('aboutOpened', true);
    },

    logout() {
      $.post(this.get('logoutUrl'))
        .always(() => window.location.href = this.get('onezoneUrl'));
    },
  },
});
