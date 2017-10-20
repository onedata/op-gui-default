/**
 * Controller used to watch if model of route changed - and then change active
 * groups menu option.
 * @module controllers/groups/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Controller,
  run,
  inject: { service },
} = Ember;

export default Controller.extend({
  secondaryMenu: service(),

  changeMenuActiveItem() {
    this.set('secondaryMenu.activeItem', this.get('model'));
    
    run.scheduleOnce('afterRender', this, function() {
      $('nav.secondary-sidebar').addClass('visible');
    });
  },

  onGroupChange: function() {
    if (this.get('model')) {
      this.changeMenuActiveItem();
    }
  },
});
