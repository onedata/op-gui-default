import Ember from 'ember';

/**
 * Base for spaces/groups submenu options controllers - select submenu option on route's
 * model change.
 *
 * @module controllers/mixins/show-permission-controller
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  secondaryMenu: Ember.inject.service(),

  changeMenuActiveOption() {
    this.set('secondaryMenu.activeOption', this.get('permissionsType'));

    Ember.run.scheduleOnce('afterRender', this, function() {
      $('nav.secondary-sidebar').removeClass('visible');
    });
  },

  onModelChange: function() {
    this.changeMenuActiveOption();
  }.observes('model')
});
