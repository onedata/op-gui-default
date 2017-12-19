/**
 * A container component for pie chart of transfers input/output throuthput
 * sum per provider.
 * 
 * @module components/transfers/throughput-distribution
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

import spaceTransfers from 'op-worker-gui/utils/transfers/space-transfers';

const {
  computed,
  Component,
  isArray,
} = Ember;

export default Component.extend({
  classNames: ['transfers-throughput-distribution'],

  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,

  /**
   * Predefined providers colors
   * @virtual
   * @type {Object}
   */
  providersColors: Object.freeze({}),
  
  /**
   * @virtual 
   * @type {Array<ProviderTransfer>}
   */
  providerTransfers: undefined,
    
  chartDataIsLoaded: computed('providers.[]', 'providerTransfers.[]', function () {
    return isArray(this.get('providers')) && isArray(this.get('providerTransfers'));
  }),
  
  /**
   * What speed (input, output) should be presented per provider
   * One of: out, in
   * @type {string}
   */
  transfersDirection: 'out',
  
  /**
   * Collection of objects containing (src, dest, speed) of transfers,
   * see `util:transfers/space-transfers` for details.
   * @type {Array<SpaceInputTransfer|SpaceOutputTransfer>}
   */
  spaceTransfers: computed('transfersDirection', 'providerTransfers.@each.bytesPerSec', function () {
    const {
      transfersDirection,
      providerTransfers,
    } = this.getProperties('transfersDirection', 'providerTransfers');
    return spaceTransfers(providerTransfers, transfersDirection);
  }),
  
  actions: {
    toggleDirection() {
      let direction = this.get('transfersDirection');
      direction = _.startsWith(direction, 'o') ? 'in' : 'out';
      this.set('transfersDirection', direction);
    },
  },
});
