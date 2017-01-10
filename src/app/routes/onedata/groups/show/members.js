/**
 * Route to collection of single user single group permissions.
 * Loads the userPermissions collection for Group loaded in group route.
 *
 * @module routes/groups/show/members
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import ShowPermissionsRouteMixin from 'op-worker-gui/mixins/show-permissions-route';
import Ember from 'ember';

export default Ember.Route.extend(ShowPermissionsRouteMixin, {
  oneproviderServer: Ember.inject.service(),

  permissionsType: 'users',
  routeType: 'groups',

  /** Override from mixin - we need two separate permissions collections */
  model() {
    var subject = this.modelFor(`onedata.${this.get('routeType')}.show`);
    return {
      subject: subject,
      // FIXME
      userPermissions: subject.get('userPermissions'),
      // FIXME
      groupPermissions: subject.get('groupPermissions'),
      availableGroups: this.modelFor('onedata.groups')
    };
  },

  actions: {
    didTransition() {
      this.controller.changeMenuActiveOption();
    },

    // TODO: implement
    // inviteItem() {
    //   let space = this.modelFor('onedata.spaces.show');
    //   this.get('oneproviderServer').inviteUser(space).then(
    //     (token) => {
    //       this.set('inviteToken', token);
    //     }
    //     // TODO: handle errors
    //   );
    // }
  }
});
