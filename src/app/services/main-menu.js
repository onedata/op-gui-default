/**
 * A global state of main menu (sidebar with "pages").
 *
 * The most important property of this service is "currentItem", which stores
 * name of currently selected item.
 *
 * Each main route should be mixed with main-route-mixin, which sets currentItem
 * of the global state on activation.
 *
 * @see {@link components/main-menu} for the component which uses this service
 * @module services/main-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  component: null,
  isVisible: true,

  /** Current route injected by application controller on currentPath change */
  currentPath: null,

  /** Parses currentPath (a route path) and sets currentItem */
  currentItem: function() {
    let itemName = this.get('currentPath').match(/lang\.((\w|-)+)\.?.*/);
    return itemName && itemName[1];
  }.property('currentPath'),

  clearSelection() {
    this.set('currentItem', null);
  },
});
