import Ember from 'ember';

/**
 * Base for spaces submenu options controllers - select submenu option on route's
 * model change.
 * @module controllers/spaces/show-permission-base
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  spacesMenu: Ember.inject.service(),

  onModelChange: function() {
    let permissionsType = this.get('permissionsType');
    Ember.run.scheduleOnce('afterRender', this, function() {
      this.set('spacesMenu.activeOption', permissionsType);
      $('nav.secondary-sidebar').removeClass('visible');
    });
  }.observes('model')
});
