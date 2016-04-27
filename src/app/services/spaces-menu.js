/**
 * Allows manipulation of spaces-menu look.
 * Delegates actions to spaces-menu component.
 *
 * Delegated API methods:
 * - selectSpace(space)
 * - clearSpaceSelection
 * - selectSubmenu(optionName)
 *
 * @see {@link components/main-menu} for the API implementation
 * @module services/main-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  component: null,

  activeSpace: null,
  activeOption: null,

  clear: function() {
    this.setProperties({
      component: null,
      activeSpace: null,
      activeOption: null,
    });
  },

  activeSpaceDidChange: function() {
    this.set('activeOption', null);
  }.observes('activeSpace'),

  componentChanged: function() {
    console.debug(`service.spaces-menu: Component changed: ${this.get('component')}`);
  }.observes('component'),
});
