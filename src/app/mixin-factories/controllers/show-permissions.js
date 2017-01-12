import Ember from 'ember';

const {
  inject,
  computed,
  observer
} = Ember;

/**
 * Base for spaces/groups submenu options controllers - select submenu option on route's
 * model change.
 *
 * @module controllers/mixins/show-permission-controller
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default create;

function create(permissionType) {
  let mixin = Ember.Mixin.create({
    secondaryMenu: inject.service(),

    subject: computed.alias('model'),
    permissions: computed.alias(`model.${permissionType}List.permissions`),
    canViewPermissions: computed.alias('subject.hasViewPrivilege'),

    changeMenuActiveOption() {
      this.set('secondaryMenu.activeOption', permissionType);

      // TODO: control visible class of secondary sidebar with bound property
      // Ember.run.scheduleOnce('afterRender', this, function() {
      //   $('nav.secondary-sidebar').removeClass('visible');
      // });
    },

    onModelChange: observer('model', function () {
      this.changeMenuActiveOption();
    }),

  });

  return mixin;
}
