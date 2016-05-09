/**
 * Route to collection of single user single group permissions.
 * Loads the userPermissions collection for Group loaded in group route.
 *
 * @module routes/groups/show/users
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import ShowPermissionsRouteMixin from '../mixins/show-permissions-route';
import Ember from 'ember';

export default Ember.Route.extend(ShowPermissionsRouteMixin, {
  oneproviderServer: Ember.inject.service(),

  // FIXME: users and groups should be loaded in this route
  permissionsType: 'users',
  routeType: 'groups',

  actions: {
    // TODO: implement
    // inviteItem() {
    //   let space = this.modelFor('spaces.show');
    //   this.get('oneproviderServer').inviteUser(space).then(
    //     (token) => {
    //       this.set('inviteToken', token);
    //     }
    //     // TODO: handle errors
    //   );
    // }
  }
});
