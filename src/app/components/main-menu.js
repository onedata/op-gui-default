/**
 * A first sidebar on the left (fixed in desktop/tablet, toggled in mobile).
 * An entry point for main routes (e.g. /data).
 * It exposes a service, which allows e.g. to highlight a sidebar item.
 *
 * Send actions:
 * - goToItem(itemName)
 *
 * @module components/main-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Component.extend({
  classNameBindings: ['isVisible:visible'],

  session: Ember.inject.service('session'),
  service: Ember.inject.service('main-menu'),

  currentItem: Ember.computed.alias('service.currentItem'),
  isVisible: Ember.computed.alias('service.isVisible'),

  registerInService: Ember.on('init', function() {
    this.set('service.component', this);
  }),

  onezoneUrl: function() {
    return this.get('session.sessionDetails.manageProvidersURL');
  }.property('session.sessionDetails'),

  didInsertElement() {
    $('nav.primary-sidebar').hover(() => {
      $('nav.primary-sidebar').toggleClass('visible');
    });
  },

  actions: {
    goToItem(itemName) {
      // menu highlight changed on route activate, see main-route-mixin
      this.sendAction('goToItem', itemName);
    }
  }
});
