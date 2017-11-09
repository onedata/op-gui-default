import Ember from 'ember';

const {
  Component,
  computed,
  get,
} = Ember;

import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
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
   * @type {SpaceTransfersUpdater}
   */
  transfersUpdater: undefined,
  
  _transfersUpdaterEnabled: true,
  
  /**
   * Collection of Transfer model for current transfers
   */
  currentTransfers: computed.reads('space.currentTransferList.list'),
  completedTransfers: computed.reads('space.completedTransferList.list'),
  // FIXME: transfers loading (private)
  // FIXME: transfers error (private)

  _currentStats: computed.reads('currentTransfers.@each.currentStat'),
  
  providers: computed.reads('space.providerList.queryList.content'),
  // FIXME: providers loading (important: yielded)
  // FIXME: providers error (important: yielded)
  
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
  
  init() {
    this._super(...arguments);
    
    const {
      space,
      _transfersUpdaterEnabled,
    } = this.getProperties('space', '_transfersUpdaterEnabled');
    
    const transfersUpdater = SpaceTransfersUpdater.create({
      isEnabled: _transfersUpdaterEnabled,
      space,
    });
    
    this.set('transfersUpdater', transfersUpdater);
  },
  
  willDestroyElement() {
    try {
      this.get('transfersUpdater').destroy();
    } finally {
      this._super(...arguments);
    }
  },
});
