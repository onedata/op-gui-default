/**
 * Route to collection of single user single space permissions.
 * Loads the userPermissions collection for Space loaded in space route.
 *
 * @module routes/spaces/show/users
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ShowPermissionsRouteMixin from '../../../mixins/show-permissions-route';

export default Ember.Route.extend(ShowPermissionsRouteMixin, {
  oneproviderServer: Ember.inject.service(),

  permissionsType: 'users',
  routeType: 'spaces',

  actions: {
    inviteItem() {
      let space = this.modelFor('spaces.show');
      this.get('oneproviderServer').inviteUser(space).then(
        (token) => {
          this.set('inviteToken', token);
        }
        // TODO: handle errors
      );
    }
  }
});
