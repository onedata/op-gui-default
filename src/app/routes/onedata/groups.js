import Ember from 'ember';
import userCollectionModel from 'ember-cli-onedata-common/mixin-factories/routes/user-collection-model';

/**
 * A Groups secondary sidebar.
 *
 * It lists groups to which user belongs to.
 * 
 * @module routes/onedata/groups
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(userCollectionModel('groups'), {
  mainRouteName: 'groups',

  actions: {
    /** Show submenu for Group */
    goToGroup(group) {
      if (group) {
        this.transitionTo('onedata.groups.show', group);
      } else {
        this.transitionTo('onedata.groups');
      }
    },

    /** Show users/groups/etc. permissions table using route */
    openSubmenuEntry(group, name) {
      console.debug(`route groups: openSubmenuEntry(${group.get('id')}, ${name})`);
      if (group && name) {
        this.transitionTo(`onedata.groups.show.${name}`, group);
      }
    }
  }

});
