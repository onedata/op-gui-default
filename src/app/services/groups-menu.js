// TODO: common mixin with groups-menu?

/**
 * Allows manipulation of groups-menu look.
 * Delegates actions to groups-menu component.
 *
 * Delegated API methods:
 * - selectGroup(group)
 * - clearGroupSelection
 * - selectSubmenu(optionName)
 *
 * @see {@link components/main-menu} for the API implementation
 * @module services/groups-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  component: null,

  activeGroup: null,
  activeOption: null,

  clear: function() {
    this.setProperties({
      component: null,
      activeGroup: null,
      activeOption: null,
    });
  },

  activeGroupDidChange: function() {

    this.set('activeOption', null);
  }.observes('activeGroup'),

  activeOptionDidChange: Ember.observer('activeOption', function () {

  }),

  componentChanged: function() {
    console.debug(`service.groups-menu: Component changed: ${this.get('component')}`);
  }.observes('component'),
});
