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
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  component: null,

  activeItem: null,
  activeOption: null,

  clear: function() {
    this.setProperties({
      component: null,
      activeItem: null,
      activeOption: null,
    });
  },

  resetActiveOption: function() {
    this.set('activeOption', null);
  }.observes('activeItem'),
});
