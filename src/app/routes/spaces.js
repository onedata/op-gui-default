/**
 * A Spaces page from main-menu.
 *
 * Could list a Spaces for user, who can select the Space and configure it.
 * @module routes/spaces
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import MainRouteMixin from '../mixins/main-route-mixin';

export default Ember.Route.extend(MainRouteMixin, {
  mainRouteName: 'spaces',

  model() {
    return this.store.findAll('space');
  },

  // afterModel moved to onSpaceChange in controller because of model load problems

  actions: {
    /** Show submenu for Space */
    goToSpace(space) {
      this.transitionTo('spaces.show', space);
    },

    /** Show users/groups/etc. permissions table using route */
    openSubmenuEntry(space, name) {
      if (space && name) {
        this.transitionTo(`spaces.show.${name}`, space);
      } else {
        console.debug(`Tried to openSubmenuEntry in spaces route, but space: '${space}', name: '${name}'`);
      }
    }
  }
});
