import Ember from 'ember';

const {
  Component,
  computed,
  get,
} = Ember;

import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';

export default Component.extend({
  classNames: ['transfers-container'],

  /**
   * @virtual
   * @type {Space}
   */
  space: undefined,

  /**
   * Collection of Transfer model for current transfers
   */
  currentTransfers: computed.reads('space.currentTransferList.list'),
  // FIXME: transfers loading (private)
  // FIXME: transfers error (private)

  providers: computed.reads('space.providerList.list.content'),
  // FIXME: providers loading (important: yielded)
  // FIXME: providers error (important: yielded)

  _currentStats: computed.reads('currentTransfers.@each.currentStat'),
  
  // FIXME: backend not implemented, using proxy.content
  transferSpeeds: computed(
    'currentTransfers.[]',
    '_currentStats.@each.bytesPerSec',
    function () {
      const transfers = this.get('currentTransfers');
      // FIXME: each transfer has currentStat loaded
      if (transfers) {
        return transfers.map(t => ({
          dest: get(t, 'destination'),
          bytesPerSec: get(t, 'currentStat.content.bytesPerSec'),
        })); 
      }
    }
  ),

  /**
   * See `util:transfers/provider-transfers` for type def. and generation
   * @type {Array<ProviderTransfer>}
   */
  providerTransfers: computed('transferSpeeds.[]', function () {
    const pt = providerTransfers(this.get('transferSpeeds'));
    return pt;
  }),

  /**
   * Collection of connection between two providers (for map display)
   * Order in connection is random; each pair can occur once.
   * See `util:transfers/provider-transfer-connections`
   * `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
   * @type {Array<ProviderTransferConnection>}
   */
  providerTransferConnections: computed('providerTransfers', function () {
    return providerTransferConnections(this.get('providerTransfers'));
  }),
});
