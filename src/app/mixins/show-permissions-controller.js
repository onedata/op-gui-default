import Ember from 'ember';

/**
 * Base for spaces/groups submenu options controllers - select submenu option on route's
 * model change.
 *
 * Abstract properties to inject:
 * - secondaryMenu - inject with secondary menu service, eg. Ember.inject.service('spacesMenu')
 *
 * @module controllers/mixins/show-permission-controller
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  /**
    Abstract: should be injected with Ember service that represents secondary
    menu service. Eg. a spacesMenu.
  */
  secondaryMenu: null,

  onModelChange: function() {
    let permissionsType = this.get('permissionsType');

    this.set('secondaryMenu.activeOption', permissionsType);
    // TODO: should use properties
    Ember.run.scheduleOnce('afterRender', this, function() {
      $('nav.secondary-sidebar').removeClass('visible');
    });
  }.observes('model', 'model.subject')
});
