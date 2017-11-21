/**
 * A container for components for showing transfers for single space.
 * 
 * It takes a space and prepares transfer data needed for rendering other components.
 * 
 * Pipeline of getting data of transfers looks like:
 * `
 * space
 *    -> currentTransfers (multi src., one dest.)
 *      -> providerTransfers (one src., one dest.)
 * `
 * 
 * 
 * @module components/transfers/transfers-container
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

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
  isEmpty,
} = Ember;

import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';

export default Component.extend({
  classNames: ['transfers-container'],
  
  session: service(),
  store: service(),
  
  /**
   * @virtual
   * @type {Space}
   */
  space: undefined,

  /**
   * @public
   * Manually enable/disable updater (eg. for testing)
   * @type {boolean}
   */
  transfersUpdaterEnabled: true,
  
  /**
   * Updates transfers data needed by most of visible components.
   * Initialized in `init`.
   * @type {SpaceTransfersUpdater}
   */
  transfersUpdater: undefined,
  
  /**
   * Private enabled/disabled state for updater - can disable updater even if
   * `transfersUpdaterEnabled` is enabled because it should not work.
   * @type {Ember.ComputedProperty<boolean>}
   */
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
  
  // FIXME: partial loader
  /**
   * Collection of Transfer model for current (active or scheduled) transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  currentTransfers: computed.reads('space.currentTransferList.list.content'),
  
  // FIXME: partial loader
  /**
   * Collection of Transfer model for completed transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  completedTransfers: computed.reads('space.completedTransferList.list.content'),
  
  /**
   * Alias for Id of this provider - used for checking if transfers can be fetched
   * @type {Ember.ComputedProperty<string>}
   */
  sessionProviderId: computed.reads('session.sessionDetails.providerId'),

  // FIXME: partial loader
  /**
   * List of providers that support this space
   * @type {Ember.ComputedProperty<Ember.Array<Provider>>}
   */
  providers: computed.reads('space.providerList.queryList.content'),
  
  /**
   * True if transfers can be listed because space is supported by current
   * provider.
   * @type {Ember.ComputedProperty<boolean>}
   */
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
   * Currently a global loader
   * FIXME: make partial loaders that does not force
   * other components to disappear and re-render
   * @type {Ember.ComputedProperty<Array<TransferCurrentStat>>}
   */
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
  
  
  /**
   * Holds current state of provider transfers array.
   * Intended to be mutable - reference should stay the same for component life.
   * Initialized to empty array on init.
   * @type {Ember.Array<ProviderTransfer>}
   */
  _providerTransfersCache: null,
  
  /**
   * Each object is a one-direction transfer from one provider to another.
   * NOTE: is empty Ember Array until currentTransfers loads.
   * (async -> currentTransfers.[], default: A([]))
   * 
   * See `util:transfers/provider-transfers` for type def. and generation
   * Mutable, array reference stays the same for component life.
   * @type {Ember.ComputedProperty<Array<ProviderTransfer>|undefined>}
   */
  providerTransfers: computed(
    'tableDataIsLoaded',
    'currentTransfers.@each.bytesPerSec',
    function getProviderTransfers() {
      if (this.get('tableDataIsLoaded')) {
        const {
          _providerTransfersCache,
          currentTransfers,
      } = this.getProperties('_providerTransfersCache', 'currentTransfers');

        this._updateProviderTransfersCache(
          _providerTransfersCache,
          currentTransfers
        );
      }

      return this.get('_providerTransfersCache');
    }
  ),
  
  /**
   * Updates (adds/removes) ProviderTransfers in cache
   * @param {Ember.Array<ProviderTransfer>} ptCache 
   * @param {Ember.Array<Transfer>} currentTransfers 
   */
  _updateProviderTransfersCache(ptCache, currentTransfers) {
    const ptList = providerTransfers(currentTransfers.toArray());
    ptCache.forEach(pt => {
      if (!_.find(ptList, { src: get(pt, 'src'), dest: get(pt, 'dest') })) {
        ptCache.removeObject(pt);
      }
    });
    ptList.forEach(pt => {
      const ptOldVer = _.find(ptCache, { src: get(pt, 'src'), dest: get(pt, 'dest') });
      if (ptOldVer) {
        set(ptOldVer, 'bytesPerSec', get(pt, 'bytesPerSec'));
      } else {
        ptCache.pushObject(pt);
      }
    });
    
    this.set('_providerTransfersCache', ptCache);
  },

  /**
   * Cache for `providerTransferConnections`
   * @type {Ember.Array<ProviderTransferConnection>}
   */
  _ptcCache: undefined,
  
  /**
   * (async -> providerTransfers)
   * Collection of connection between two providers (for map display)
   * Order in connection is random; each pair can occur once.
   * See `util:transfers/provider-transfer-connections`
   * `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
   * @type {Ember.ComputedProperty<Array<ProviderTransferConnection|undefined>>}
   */
  providerTransferConnections: computed(
    'providerTransfers',
    '_ptcCache',
    function getProviderTransferConnections() {
      const providerTransfers = this.get('providerTransfers');
      let _ptcCache = this.get('_ptcCache');
      if (providerTransfers) {
        mutateArray(
          _ptcCache,
          providerTransferConnections(providerTransfers),
          (x, y) => x[0] === y[0] && x[1] === y[1]
        );
      }
      return _ptcCache;
    }
  ),
  
  /**
   * Creates an array of provider ids that are destination of transfers for space
   * NOTE: returns new array every recomputation
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  destinationProviderIds: computed(
    'currentTransfers.@each.destination',
    function getDestinationProviderIds() {
      const transfers = this.get('currentTransfers');
      if (!isEmpty(transfers)) {
        return _.uniq(transfers.map(t => get(t, 'destination'))); 
      }
    }
  ),
  
  /**
   * Creates an array of provider ids that are destination of transfers for space
   * NOTE: returns new array every recomputation
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  sourceProviderIds: computed(
    'currentTransfers.@each.bytesPerSec',
    function getSourceProviderIds() {
      const transfers = this.get('currentTransfers');
      if (!isEmpty(transfers)) {
        return _(transfers)
          .map(t => {
            if (t && get(t, 'bytesPerSec')) {
              return Object.keys(get(t, 'bytesPerSec'));
            } else {
              return [];
            }
          })
          .flatten()
          .uniq()
          .value();
      }
    }
  ),
  
  /**
   * Watches updater settings dependecies and changes its settings
   */
  configureTransfersUpdater: observer(
    '_transfersUpdaterEnabled',
    'space',
    function configureTransfersUpdater() {
      const {
        _transfersUpdaterEnabled,
        space,
      } = this.getProperties(
        '_transfersUpdaterEnabled',
        'space'
      );
      this.get('transfersUpdater').setProperties({
        isEnabled: _transfersUpdaterEnabled,
        space: space,
      });
    }
  ),
  
  init() {
    this._super(...arguments);
    const {
      _transfersUpdaterEnabled,
      space,
      store,
    } = this.getProperties(
      '_transfersUpdaterEnabled',
      'space',
      'store'
    );
    
    const transfersUpdater = SpaceTransfersUpdater.create({
      store,
      isEnabled: _transfersUpdaterEnabled,
      space: space,
    });    
    this.set('transfersUpdater', transfersUpdater);
    
    this._initializeDefaultValues();
  },
  
  _initializeDefaultValues() {
    this.setProperties({
      _ptcCache: A(),
      _providerTransfersCache: A(),
    });
  },
  
  willDestroyElement() {
    try {
      this.get('transfersUpdater').destroy();
    } finally {
      this._super(...arguments);
    }
  },
});
