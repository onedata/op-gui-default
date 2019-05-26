/**
 * Global state of secondary menu (secondary sidebar).
 * Secondary menu consists of items (first-level) and options (second-level).
 * Items are representation of model (records in array).
 * Options are always the same for each record.
 *
 * Components which use secondary-menu are using following properties:
 * - component {Ember.Component}
 * - activeItem {model}
 * - activeOption {string}
 *
 * Users of the service observes and aliases these properties to perform actions
 * and change its look when item/option changes.
 *
 * @module services/secondary-menu
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  computed,
} = Ember;

export default Ember.Service.extend({  
  component: null,

  activeItem: null,
  activeOption: null,

  itemType: computed('activeItem', function() {
    let item = this.get('activeItem');
    if (item) {
      console.debug(`item type: ${item.constructor.toString()}`);
    }
    return item && (
      item.constructor.toString().match(/model:((\w|-)+)/)[1] ||
      item.constructor.toString() ||
      undefined
    );
  }),

  /// specific item getters - this is a "syntatic sugar" to pass the itemType check

  activeSpace: computed('activeItem', 'itemType', {
    get() {
      return this.get('itemType') === 'space' ?
        this.get('activeItem') : null;
    },
    set(key, value) {
      return this.set('activeItem', value);
    }
  }),
  
  activeGroup: computed('activeItem', 'itemType', {
    get() {
      return this.get('itemType') === 'group' ?
        this.get('activeItem') : null;
    },
    set(key, value) {
      return this.set('activeItem', value);
    }
  }),
  
  activeShare: computed('activeItem', 'itemType', {
    get() {
      return this.get('itemType') === 'share' ?
        this.get('activeItem') : null;
    },
    set(key, value) {
      return this.set('activeItem', value);
    }
  }),
  
  clear: function() {
    this.setProperties({
      component: null,
      activeItem: null,
      activeOption: null,
    });
  },
});
