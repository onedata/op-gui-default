import Ember from 'ember';

/**
 * Controller used to watch if model of route changed - and then change active
 * groups menu option.
 * @module controllers/groups/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  secondaryMenu: Ember.inject.service(),

  changeMenuActiveItem() {
    this.set('secondaryMenu.activeItem', this.get('model'));
  },

  onGroupChange: function() {
    if (this.get('model')) {
      this.changeMenuActiveItem();
    }
  }.observes('model'),
});
