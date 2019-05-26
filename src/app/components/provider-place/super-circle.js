/**
 * An animated cirlce that should be rendered below provider circle.
 * 
 * It denotes that provider is transferring data: in or out.
 *
 * @module components/provider-place/super-circle
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
  String: {
    htmlSafe,
  },
} = Ember;

export default Component.extend({
  classNames: ['super-circle'],
  classNameBindings: ['isSource:source', 'isDestination:destination'],
  attributeBindings: ['style'],
  
  /**
   * @type {boolean}
   */
  isSource: false,
  
  /**
   * @type {boolean}
   */
  isDestination: false,

  /**
   * @virtual
   * @type {string}
   */
  circleColor: '',

  /**
   * Computed provider-specific style
   * @type {Ember.ComputedProperty<string>}
   */
  style: computed('circleColor', function () {
    const circleColor = this.get('circleColor');
    return htmlSafe(`background-color: ${circleColor};`);
  }),
});
