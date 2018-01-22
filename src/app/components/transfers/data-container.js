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
 * @module components/transfers/data-container
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
  observer,
  isEmpty,
  run,
} = Ember;

import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import generateColors from 'op-worker-gui/utils/generate-colors';

const RE_TRANSFER_ROW_ID = /transfer-row-(.*)/;

export default Component.extend({
  classNames: ['transfers-data-container'],
  
  session: service(),
  store: service(),
  
  /**
   * @virtual
   * @type {Space}
   */
  space: undefined,
  
  /**
   * @virtual
   * @type {Array<string>|undefined}
   */
  selectedTransferIds: undefined,

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
  
  _transfersUpdaterEnabled: computed.readOnly('transfersUpdaterEnabled'),
  
  //#region Space properties aliases
  
  currentTransferList: computed.reads('space.currentTransferList'),
  completedTransferList: computed.reads('space.completedTransferList'),
  providerList: computed.reads('space.providerList'),
  
  //#endregion
  
  /**
   * Collection of Transfer model for current
   * (active, invalidating or scheduled) transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  currentTransfers: computed.reads('currentTransferList.list.content'),
  
  /**
   * Collection of Transfer model for completed transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  completedTransfers: computed.reads('completedTransferList.list.content'),

  /**
   * List of providers that support this space
   * @type {Ember.ComputedProperty<Ember.Array<Provider>>}
   */
  providers: computed.reads('providerList.queryList.content'),
  
  //#region Loading and error states of yielded values
  
  providersLoaded: computed.reads('providerList.queryList.isSettled'),
  providersError: computed.reads('providerList.queryList.reason'),

  currentTransfersLoaded: computed(
    'currentTransferList.isLoaded',
    'currentTransfers.isLoaded',
    function getCurrentTransfersLoaded() {
      return this.get('currentTransferList.isLoaded') === true &&
        this.get('currentTransfers.isLoaded') === true;
    }
  ),
  
  completedTransfersLoaded: computed(
    'completedTransferList.isLoaded',
    'completedTransfers.isLoaded',
    function getCompletedTransfersLoaded() {
      return this.get('completedTransferList.isLoaded') === true &&
        this.get('completedTransfers.isLoaded') === true;
    }
  ),
  
  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  allTablesLoaded: computed(
    'providersLoaded',
    'currentTransfersLoaded',
    'completedTransfersLoaded',
    function () {
      return this.get('providersLoaded') &&
        this.get('currentTransfersLoaded'),
        this.get('completedTransfersLoaded');
    }
  ),
  
  //#endregion
  
  /**
   * Holds current state of provider transfers array.
   * Intended to be mutable - reference should stay the same for component life.
   * Initialized to empty array on init.
   * @type {Ember.Array<ProviderTransfer>}
   */
  _providerTransfersCache: null,
  
  /**
   * If true, this instance of data container already scrolled to selected transfers
   * @type {boolean}
   */
  _scrolledToSelectedTransfers: false,
  
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
    'currentTransfersLoaded',
    'currentTransfers.@each.bytesPerSec',
    function getProviderTransfers() {
      if (this.get('currentTransfersLoaded')) {
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
    const ptNewList = providerTransfers(currentTransfers.toArray());
    _.remove(ptCache, pt =>
      !_.find(ptNewList, { src: get(pt, 'src'), dest: get(pt, 'dest') })
    );
    ptNewList.forEach(pt => {
      const ptOldVer = _.find(ptCache, { src: get(pt, 'src'), dest: get(pt, 'dest') });
      if (ptOldVer) {
        set(ptOldVer, 'bytesPerSec', get(pt, 'bytesPerSec'));
      } else {
        ptCache.pushObject(pt);
      }
    });
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
        return _.uniq(_.flatten(transfers
          .map(t => {
            if (t && get(t, 'bytesPerSec')) {
              return Object.keys(get(t, 'bytesPerSec'));
            } else {
              return [];
            }
          })));
      }
    }
  ),

  /**
   * Global colors for each provider
   * @type {Ember.ComputedProperty<Object>}
   */
  providersColors: computed('providers.@each.id', function () {
    const providers = this.get('providers');
    if (providers) {
      const providerIds = providers.mapBy('id').sort();
      const colors = generateColors(providerIds.length);
      return _.zipObject(providerIds, colors);
    }
  }),
  
  /**
   * True if at least one current transfer's current stat cannot be loaded
   * @type {boolean}
   */
  throughputChartError: computed('currentTransfers.@each.currentStatError', function () {
    const currentTransfers = this.get('currentTransfers');
    if (currentTransfers) {
      // using every, because this kind of array doesn't have "some"
      return !currentTransfers.every(t => !t || !get(t, 'currentStatError'));
    }
  }),
  
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
    
  observeScrollToSelectedTransfers: observer('allTablesLoaded', function () { 
    if (this.get('_scrolledToSelectedTransfers') === false && this.get('allTablesLoaded')) {
      run.next(() => this._scrollToFirstSelectedTransfer());
      this.set('_scrolledToSelectedTransfers', true);
    }
  }),
  
  init() {
    this._super(...arguments);
    const {
      _transfersUpdaterEnabled,
      space,
      store,
    } = this.getProperties(
      '_transfersUpdaterEnabled',
      'space',
      'store',
      // just enable observers
      'allTablesLoaded'
    );
    
    const transfersUpdater = SpaceTransfersUpdater.create({
      store,
      isEnabled: _transfersUpdaterEnabled,
      space: space,
    });    
    this.set('transfersUpdater', transfersUpdater);
    
    this._initializeDefaultValues();
  },
  
  _scrollToFirstSelectedTransfer() {
    const selectedTransferIds = this.get('selectedTransferIds');
    
    const trs = this.$('tr.transfer-row').toArray();
    for (let i = 0; i < trs.length; i++) {
      const transferElement = trs[i];
      const tid = transferElement.id.match(RE_TRANSFER_ROW_ID)[1];
      if (_.includes(selectedTransferIds, tid)) {
        // estimate height of top toolbar + height of the table header
        // (it's better to present table header if possible)
        let navHeight;
        let thHeight;
        try {
          navHeight = parseInt(
            window.getComputedStyle($('header')[0])
              .getPropertyValue('height')
          );
          thHeight = parseInt(
            window.getComputedStyle($('.transfers-live-stats-table thead')[0])
              .getPropertyValue('height')
          );
        } catch (error) {
          console.warn(
            'component:transfers/data-container: an error occured when ' + 
            'computing scrolling offset, falling back to default'
          );
          console.warn(error);
          navHeight = 80;
          thHeight = 52;
        }
        $('#content-scroll').scrollTop($(transferElement).offset().top - (navHeight + thHeight));
        break;
      }
    }
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
