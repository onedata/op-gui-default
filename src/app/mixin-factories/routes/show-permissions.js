/**
 * A factory of mixins for routes that show configurable list of permissions.
 *
 * @module mixin-factories/routes/show-permissions
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  on,
  run
} = Ember;

/**
 * @param {string} routeType eg. space, group
 * @param {string} permissionType eg. user, group
 */
function create(routeType) {
  let mixin = Ember.Mixin.create({
    model() {
      return this.modelFor(`onedata.${routeType}.show`);
    },

    setupController(controller, model) {
      this._super(controller, model);
      controller.changeMenuActiveOption();
    },

    onDeactivate: on('deactivate', function() {
      run.scheduleOnce('afterRender', this, function() {
        $('nav.secondary-sidebar').addClass('visible');
      });
    }),

    actions: {
      /** Change state of single permission checkbox */
      togglePermission: function(permission, propertyName) {
        let permName = 'perm' + propertyName;
        let modName = 'mod' + propertyName;
        permission.toggleProperty(permName);
        permission.toggleProperty(modName);
      },

      reload: function() {
        this.refresh();
      }
    }
  });

  return mixin;
}


export default create;
