/**
 * Route to collection of single Group single Space permissions.
 * Loads the userPermissions collection for Space loaded in space route.
 *
 * @module routes/spaces/show/groups
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import showPermissionsMixinFactory from 'op-worker-gui/mixin-factories/routes/show-permissions';

export default Ember.Route.extend(showPermissionsMixinFactory('spaces'), {
  oneproviderServer: Ember.inject.service(),

  actions: {
    inviteItem() {
      let space = this.modelFor('onedata.spaces.show');
      this.get('oneproviderServer').inviteGroup(space).then(
        (token) => {
          this.set('inviteToken', token);
        }
        // TODO: handle errors
      );
    }
  }
});
