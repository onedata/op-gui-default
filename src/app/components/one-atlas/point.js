/**
 * An one-atlas point component. Represents a place in a map, which 
 * should be specified with latitude and longitude properties.
 * Example of use can be found in one-atlas component.
 * 
 * Module imported from onedata-gui-common.
 * 
 * @module components/one-atlas/point
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  observer,
} = Ember;

export default Component.extend({
  classNames: ['one-atlas-point'],

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
  positionX: undefined,

  /**
   * Point y position on atlas (in px)
   * @virtual
   * @type {number}
   */
  positionY: undefined,

  _positionObserver: observer('positionX', 'positionY', function () {
    this._applyPosition();
  }),

  didInsertElement() {
    this._super(...arguments);
    this._applyPosition();
  },

  /**
   * Applies calculated position to CSS properties
   */
  _applyPosition() {
    const {
      positionX,
      positionY,
    } = this.getProperties('positionX', 'positionY');

    this.$().css({
      top: positionY,
      left: positionX,
    });
  }
});
