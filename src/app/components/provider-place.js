/**
 * A circle representing a provider on world map.
 * 
 * @module components/provider-place
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  computed,
  observer,
  get,
} = Ember;

export default Ember.Component.extend({
  classNames: ['provider-place'],
  classNameBindings: ['status'],

  /**
   * A provider model that will be represented on map
   * To inject.
   * @type {Onezone.ProviderDetails}
   */
  provider: null,

  /**
   * Parent atlas component width
   * @type {number}
   */
  atlasWidth: 0,

  /**
   * Scale factor for circle size
   * @type {number} 
   */
  circleSizeScale: 1,

  /**
   * Provider status
   * @type {computed.string}
   */
  status: computed.readOnly('provider.status'),

  atlasWidthObserver: observer('atlasWidth', function () {
    this._recalculateSize();
  }),

  didInsertElement() {
    this._super(...arguments);
    this._recalculateSize();
  },

  /**
   * Adjusts circle size to atlas width
   */
  _recalculateSize() {
    let {
      atlasWidth,
      circleSizeScale,
    } = this.getProperties('atlasWidth', 'circleSizeScale');
    let width = atlasWidth * 0.02 * circleSizeScale;

    this.$().find('.circle').css({
      fontSize: width * 0.75 + 'px',
      width: width + 'px',
      height: width + 'px',
    });
  },
});
