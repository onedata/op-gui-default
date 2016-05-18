import Ember from 'ember';

/**
 * FIXME: doc
 * @module mixins/main-route-mixin
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  mainMenu: Ember.inject.service(),

  /** Short name of route, eg. "spaces" - must be implemented in subclasses */
  mainRouteName: null,

  onActivate: Ember.on('activate', function() {
    console.debug(`activate menu: ${this.get('mainRouteName')}`);
    this.setProperties({
      'mainMenu.currentItem': this.get('mainRouteName'),
      'mainMenu.isVisible': true
    });
  }),

});
