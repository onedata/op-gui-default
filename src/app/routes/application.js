/**
 * A main route, setting up whole application.
 * @module routes/spaces
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Route.extend({
  mainMenuService: Ember.inject.service('main-menu'),
  session: Ember.inject.service('session'),

  activate() {
    console.debug('app activate');
  },

  actions: {
    goToItem(name) {
      this.transitionTo(`${name}.index`);
    }
  },

  initSession: function () {
    // @todo This returns a promise. We should display a loading page here
    // and transition to proper page on promise resolve.
    this.get('session').initSession(false).then(
      () => {
        console.log('initSession resolved');
      }
    );
  }.on('init')
});
