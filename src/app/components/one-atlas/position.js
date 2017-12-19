/**
 * A one-atlas point component. Represents a place in a map, which 
 * should be specified with latitude and longitude properties.
 * Example of use can be found in one-atlas component.
 * 
 * @module components/one-atlas/point
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({
  tagName: '',

  /**
   * Atlas parent component width.
   * To inject by one-atlas component.
   * @type {number}
   */
  atlasWidth: 0,

  /**
   * Atlas parent component height.
   * To inject by one-atlas component.
   * @type {number}
   */
  atlasHeight: 0,

  /**
   * Point x position on atlas (in px)
   * @virtual
   * @type {number}
   */
  positionX: computed('atlasWidth', 'longitude', function () {
    let {
      atlasWidth,
      longitude,
    } = this.getProperties('atlasWidth', 'longitude');

    if (longitude < -180 || longitude > 180) {
      longitude = 0;
    }
    return ((longitude + 180) / 360) * atlasWidth;
  }),

  /**
   * Point y position on atlas (in px)
   * @virtual
   * @type {number}
   */
  positionY: computed('atlasHeight', 'latitude', function () {
    let {
      atlasHeight,
      latitude,
    } = this.getProperties('atlasHeight', 'latitude');

    if (latitude < -90 || latitude > 90) {
      latitude = 0;
    }
    // Calculations based on https://en.wikipedia.org/wiki/Mercator_projection
    // article
    let ltr = latitude * (Math.PI / 180);
    let y = 1.25 * Math.log(Math.tan(Math.PI / 4 + 0.4 * ltr));
    return (atlasHeight / 2) * (1 - y * (1 / 2.303412543));
  }),

  didInsertElement() {
    this._super(...arguments);
  },
});
