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
  providersColors: {},
  
  /**
   * @virtual 
   * @type {Array<ProviderTransfer>}
   */
  providerTransfers: undefined,
  
  // FIXME: is loading state
  
  chartDataIsLoaded: computed('providers.[]', 'providerTransfers.[]', function () {
    return isArray(this.get('providers')) && isArray(this.get('providerTransfers'));
  }),
  
  /**
   * Transfer direction. Valid values: in, out
   * @virtual
   * @type {string}
   */
  transfersDirection: 'out',
  
  /**
   * FIXME: provide this collection - multiple for single source
   * only one for destination!
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
