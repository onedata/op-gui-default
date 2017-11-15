import Ember from 'ember';
import _ from 'lodash';

const {
  Component,
  computed,
  get,
  A,
  set,
  inject: { service },
  isArray,
  observer,
} = Ember;

import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';

export default Component.extend({
  classNames: ['transfers-container'],
  
  session: service(),
  
  /**
   * @virtual
   * @type {Space}
   */
  space: undefined,

  /**
   * @type {SpaceTransfersUpdater}
   */
  transfersUpdater: undefined,
  
  /**
   * Manually enable/disable updater (eg. for testing)
   * @type {boolean}
   */
  transfersUpdaterEnabled: true,
  
  _transfersUpdaterEnabled: computed(
    'transfersUpdaterEnabled',
    'isSupportedByCurrentProvider',
    function () {
      const {
        transfersUpdaterEnabled,
        isSupportedByCurrentProvider,
      } = this.getProperties(
        'transfersUpdaterEnabled',
        'isSupportedByCurrentProvider'
      );
      return transfersUpdaterEnabled && isSupportedByCurrentProvider;
    }
  ),
  
  /**
   * Collection of Transfer model for current transfers
   */
  // FIXME: debug code - changed current with completed
  currentTransfers: computed.reads('space.currentTransferList.list.content'),
  completedTransfers: computed.reads('space.completedTransferList.list.content'),
  // FIXME: transfers loading (private)
  // FIXME: transfers error (private)

  sessionProviderId: computed.reads('session.sessionDetails.providerId'),
  isSupportedByCurrentProvider: computed('sessionProviderId', 'providers.[]', function () {
    const {
      providers,
      sessionProviderId,
    } = this.getProperties('sessionProviderId', 'providers');
    if (isArray(providers) && sessionProviderId != null) {
      return _.includes(providers.map(p => get(p, 'id')), sessionProviderId);
    } else {
      return null;
    }
  }),
  
  /**
   * @type {Ember.ComputedProperty<Array<TransferCurrentStat>>}
   */
  // FIXME: inverted current-completed
  // FIXME: it shows true when transfers list is not loaded yet
  // _currentStats: computed.mapBy('completedTransfers', 'currentStat'),
  tableDataIsLoaded: computed(
    'currentTransfers.isLoaded',
    'currentTransfers.@each.tableDataIsLoaded',
    'space.providerList.queryList.isSettled',
    'space.completedTransferList.list.isLoaded',
    function () {
      return this.get('space.completedTransferList.list.isLoaded') &&
        this.get('space.providerList.queryList.isSettled'),
        this.get('currentTransfers').every(t => get(t, 'tableDataIsLoaded') === true);
    }
  ),
  
  _completedStats: computed.mapBy('completedTransfers', 'currentStat'),
  
  
  providers: computed.reads('space.providerList.queryList.content'),
  // FIXME: providers loading (important: yielded)
  // FIXME: providers error (important: yielded)
    
  // FIXME: cache for providerTransfers
  
  _providerTransfersCache: null,
  
  /**
   * (async -> currentTransfers.[], default: A([]))
   * See `util:transfers/provider-transfers` for type def. and generation
   * @type {Ember.ComputedProperty<Array<ProviderTransfer>|undefined>}
   */
  providerTransfers: computed('tableDataIsLoaded', 'currentTransfers.[]', function () {
    if (this.get('tableDataIsLoaded')) {
      let ptCache = this.get('_providerTransfersCache');
      if (ptCache == null) {
        ptCache = A([]);
      }
      
      const currentTransfers = this.get('currentTransfers');
      const ptList = providerTransfers(currentTransfers.toArray());
      ptList.forEach(pt => {
        const ptOldVer = _.find(ptCache, { src: get(pt, 'src'), dest: get(pt, 'dest') });
        if (ptOldVer) {
          set(ptOldVer, 'bytesPerSec', get(pt, 'bytesPerSec'));
        } else {
          ptCache.push(pt);
        }
      });
      
      this.set('_providerTransfersCache', ptCache);
    }
    
    return this.get('_providerTransfersCache');
  }),

  /**
   * (async -> providerTransfers)
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
  
  toggleTransfersUpdater: observer('_transfersUpdaterEnabled', function () {
    this.set('transfersUpdater.isEnabled', this.get('_transfersUpdaterEnabled'));
  }),
  
  init() {
    this._super(...arguments);
    
    const {
      space,
      _transfersUpdaterEnabled,
    } = this.getProperties('space', '_transfersUpdaterEnabled');
    
    this.set('_providerTransfersCache', A());
    
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
