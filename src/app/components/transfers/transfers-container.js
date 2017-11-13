import Ember from 'ember';
import _ from 'lodash';

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

  /**
   * @type {Ember.ComputedProperty<Array<TransferCurrentStat>>}
   */
  _currentStats: computed.mapBy('currentTransfers', 'currentStat'),
  
  _completedStats: computed.mapBy('completedTransfers', 'currentStat'),
  
  providers: computed.reads('space.providerList.queryList.content'),
  // FIXME: providers loading (important: yielded)
  // FIXME: providers error (important: yielded)
  
  // FIXME: backend not implemented, using proxy.content
  /**
   * @type {Ember.ComputedProperty<InputTransferSpeed>}
   */
  // FIXME: temporarily changed do completed states
  transferSpeeds: computed(
    'currentTransfers.[]',
    '_currentStats.@each.isSettled',
    function () {
      const transfers = this.get('currentTransfers');
      // FIXME: each transfer has currentStat loaded
      if (transfers && _.every(this.get('_currentStats'), s => get(s, 'isSettled'))) {
        var ts = transfers.map(t => ({
          dest: get(t, 'destination'),
          bytesPerSec: get(t, 'currentStat.bytesPerSec'),
        })); 
        console.debug('debug me');
        return ts;
      }
    }
  ),

  /**
   * See `util:transfers/provider-transfers` for type def. and generation
   * @type {Ember.ComputedProperty<Array<ProviderTransfer>|undefined>}
   */
  providerTransfers: computed('transferSpeeds.[]', function () {
    const transferSpeeds = this.get('transferSpeeds');
    if (transferSpeeds) {
      const pt = providerTransfers(this.get('transferSpeeds'));
      console.debug('debug me');
      return pt;
    }
  }),

  /**
   * Collection of connection between two providers (for map display)
   * Order in connection is random; each pair can occur once.
   * See `util:transfers/provider-transfer-connections`
   * `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
   * @type {Ember.ComputedProperty<Array<ProviderTransferConnection|undefined>>}
   */
  providerTransferConnections: computed('providerTransfers', function () {
    // FIXME: debugging
    const providerTransfers = this.get('providerTransfers');
    if (providerTransfers) {
      var x = providerTransferConnections(providerTransfers);
      console.debug('debug me');
      return x; 
    }
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
