/**
 * Route to collection of single user single group permissions.
 * Loads the userPermissions collection for Group loaded in group route.
 *
 * @module routes/groups/show/members
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import showPermissionsMixinFactory from 'op-worker-gui/mixin-factories/routes/show-permissions';

export default Ember.Route.extend(showPermissionsMixinFactory('groups'), {
  oneproviderServer: Ember.inject.service(),

  actions: {
    didTransition() {
      this.controller.changeMenuActiveOption();
    },
  }
});
