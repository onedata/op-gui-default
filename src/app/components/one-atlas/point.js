/**
 * An one-atlas point component. Represents a place in a map, which 
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
  computed,
  observer,
} = Ember;

export default Ember.Component.extend({
  classNames: ['one-atlas-point'],

  /**
   * Latitude (values -90 <= x <= 90, South < North)
   * To inject.
   * @type {number}
   */
  latitude: 0,

  /**
   * Longitude (values -180 <= x <= 180, West < East)
   * To inject.
   * @type {number}
   */
  longitude: 0,

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
   * @type {computed.number}
   */
  _positionX: computed('atlasWidth', 'longitude', function () {
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
   * @type {computed.number}
   */
  _positionY: computed('atlasHeight', 'latitude', function () {
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

  _coordinatesValidatorObserver: observer('longitude', 'latitude', function () {
    let {
      longitude,
      latitude,
    } = this.getProperties('longitude', 'latitude');
    if (longitude < -180 || longitude > 180) {
      console.warn(`one-atlas/point: longitude out of range: ${longitude}`);
    }
    if (latitude < -90 || latitude > 90) {
      console.warn(`one-atlas/point: latitude out of range: ${latitude}`);
    }
  }),

  _positionObserver: observer('_positionX', '_positionY', function () {
    this._applyPosition();
  }),

  init() {
    this._super(...arguments);
    this._coordinatesValidatorObserver();
  },

  didInsertElement() {
    this._super(...arguments);
    this._applyPosition();
  },

  /**
   * Applies calculated position to CSS properties
   */
  _applyPosition() {
    let {
      _positionX,
      _positionY,
    } = this.getProperties('_positionX', '_positionY');

    this.$().css({
      top: _positionY,
      left: _positionX,
    });
  }
});
