/**
 * A Spaces page from main-menu.
 *
 * Could list a Spaces for user, who can select the Space and configure it.
 * @module routes/onedata/spaces
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import userCollectionModel from 'ember-cli-onedata-common/mixin-factories/routes/user-collection-model';

export default Ember.Route.extend(userCollectionModel('spaces', { nonEmpty: true }), {
  mainRouteName: 'spaces',

  actions: {
    /** Show submenu for Space */
    goToSpace(space) {
      this.transitionTo('onedata.spaces.show', space);
    },

    /** Show users/groups/etc. permissions table using route */
    openSubmenuEntry(space, name) {
      console.debug(`route spaces: openSubmenuEntry(${space.get('id')}, ${name})`);
      if (space && name) {
        this.transitionTo(`onedata.spaces.show.${name}`, space);
      } else {
        console.debug(`Tried to openSubmenuEntry in spaces route, but space: '${space}', name: '${name}'`);
      }
    }
  }
});
