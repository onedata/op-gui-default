import Ember from 'ember';

/**
 * Used to notify main-menu service about path changes.
 * @module controllers/appliction
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  mainMenu: Ember.inject.service(),

  updateCurrentPath: function() {
    this.set('mainMenu.currentPath', this.get('currentPath'));
  }.observes('currentPath')
});
