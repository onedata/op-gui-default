/**
 * A view component for onedata.transfers.show route
 *
 * @module components/show-space-transfers
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import generateColors from 'op-worker-gui/utils/generate-colors';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import ArraySlice from 'ember-cli-onedata-common/utils/array-slice';

const RE_TRANSFER_ROW_ID = /transfer-row-(.*)/;

const {
  Component,
  computed,
  observer,
  isArray,
  get,
  set,
  A,
  isEmpty,
  inject: { service },
  run,
} = Ember;

// FIXME: refactor, to new file
class ViewTester {
  /**
   * @param {HTMLElement} container 
   */
  constructor($container) {
    this.containerTop = $container.offset().top;
    this.containerBottom = this.containerTop + $container[0].clientHeight;
  }

  /**
   * @param {HTMLElement} elem
   */
  isInView(elem) {
    const elemTop = $(elem).offset().top;
    const elemBottom = elemTop + elem.offsetHeight;

    return (elemTop <= this.containerBottom) && (elemBottom >= this.containerTop);
  }
}

// FIXME: refactor, to new file
class ListWatcher {
  /**
   * @param {jQuery} container 
   * @param {string} itemsSelector 
   * @param {function} callback
   */
  constructor($container, itemsSelector, callback) {
    this.$container = $container;
    this.itemsSelector = itemsSelector;
    this.callback = callback;
    this._scrollHandler = this.scrollHandler.bind(this);
    this.viewTester = new ViewTester($container);

    $container.on('scroll', this._scrollHandler);
  }

  scrollHandler() {
    const items = this.$container.find(this.itemsSelector).toArray();
    let visibleFragment = false;
    const visibleElements = [];
    for (let i = 0; i < items.length; i++) {
      const item = items[i];
      const visible = this.viewTester.isInView(item);
      if (visible) {
        visibleElements.push(item);
        visibleFragment = true;
      } else if (visibleFragment) {
        break;
      }
    }
    this.callback(visibleElements);
  }

  destroy() {
    this.$container.off('scroll', this._scrollHandler);
  }
}

export default Component.extend({
  classNames: ['show-space-transfers', 'row'],
  session: service(),
  store: service(),
  
  //#region External properties
  
  /**
   * Space model, which transfers will be listed
   * @virtual
   * @type {Space}
   */
  space: undefined,
  
  /**
   * Name of transfer table column to sort by default (eg. path)
   * @type {string|undefined}
   */
  sortBy: undefined,
  
  /**
   * Ids of transfers that should be expanded, "blinked" and scrolled to
   * on entering view
   * @type {Array<string>|undefined}
   */
  selectedTransferIds: undefined,
    
  //#endregion
  
  //#region Internal properties
    
  initialActiveTabId: 'current',
  
  activeTabId: computed.reads('initialActiveTabId'),
  
  //#endregion
  
  //#region Computed properties
  
  /**
   * Alias for Id of this provider - used for checking if transfers can be fetched
   * @type {Ember.ComputedProperty<string>}
   */
  sessionProviderId: computed.reads('session.sessionDetails.providerId'),
  
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
  
  //#endregion
      
  //#region Feature: transfers data container
  
  transfersUpdaterEnabled: true,
  
  _initTransfersData() {
    const {
      _transfersUpdaterEnabled,
      space,
      store,
      activeTabId,
    } = this.getProperties(
      '_transfersUpdaterEnabled',
      'space',
      'store',
      'activeTabId',
      // just enable observers
      'allTablesLoaded'
    );
    
    const transfersUpdater = SpaceTransfersUpdater.create({
      store,
      isEnabled: _transfersUpdaterEnabled,
      currentEnabled: activeTabId === 'current',
      completedEnabled: activeTabId === 'completed',
      space: space,
    });    
    this.set('transfersUpdater', transfersUpdater);
    
    this._initializeDefaultValues();
    
    return transfersUpdater;
  },
  
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
  
  observeScrollToSelectedTransfers: observer('allTablesLoaded', function () { 
    if (this.get('_scrolledToSelectedTransfers') === false && this.get('allTablesLoaded')) {
      run.next(() => this._scrollToFirstSelectedTransfer());
      this.set('_scrolledToSelectedTransfers', true);
    }
  }),
  
  /**
   * True if at least one current transfer is controlled by remote provider
   * so statistics can be slightly delayed.
   * @type {Ember.ComputedProperty<boolean>}
   */
  someTransfersRemote: computed(
    'currentTransfers.@each.{type,status}',
    'providerId',
    function getSomeTransfersRemote() {
      const providerId = this.get('providerId');
      return this.get('currentTransfers').toArray().some(t => !t.getIsLocal(providerId));
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
  
  /**
   * Updates transfers data needed by most of visible components.
   * Initialized in `init`.
   * @type {SpaceTransfersUpdater}
   */
  transfersUpdater: undefined,
  
  providerId: computed.reads('session.sessionDetails.providerId'),
  
  _transfersUpdaterEnabled: computed.readOnly('transfersUpdaterEnabled'),
  
  currentTransferList: computed.reads('space.currentTransferList'),
  completedTransferList: computed.reads('space.completedTransferList'),
  providerList: computed.reads('space.providerList'),
    
  /**
   * Collection of Transfer model for current
   * (active, invalidating or scheduled) transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  currentTransfers: undefined,
  
  /**
   * Collection of Transfer model for completed transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  completedTransfers: undefined,
    
  providersLoaded: computed.reads('providerList.queryList.isSettled'),
  providersError: computed.reads('providerList.queryList.reason'),

  currentTransfersLoaded: computed.alias('currentTransferList.isLoaded'),

  completedTransfersLoaded: computed.alias('completedTransferList.isLoaded'),
  
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
      return currentTransfers.toArray().some(t => !t || get(t, 'currentStatError'));
    }
  }),
  
  //#endregion
  
  //#region FIXME: Unclassified
  
  enableWatcherCollection: observer('activeTabId', function enableWatcherCollection() {
    const {
      activeTabId,
      transfersUpdater,
    } = this.getProperties('activeTabId', 'transfersUpdater');    
    transfersUpdater.setProperties({
      currentEnabled: activeTabId === 'current',
      completedEnabled: activeTabId === 'completed',
    });
  }),
  
  //#endregion
  
  init() {
    this._super(...arguments);
    const transfersUpdater = this._initTransfersData();
    ['current', 'completed'].forEach(type => {
      const listRecord = this.get(`${type}TransferList`);
      const slice = ArraySlice.create({
        sourceArray: get(listRecord, 'list.content'),
        startIndex: 0,
        endIndex: 0,
        indexMargin: 15,
      });
      this.set(`${type}Transfers`, slice);
      listRecord.then(() => {
        const visibleIds = slice.mapBy('id');
        transfersUpdater.fetchSpecificRecords(visibleIds);
      });
    });
  },
  
  didInsertElement() {
    const listWatcher = new ListWatcher(
      $('#content-scroll'),
      '.transfer-row',
      items => safeExec(this, 'onTableScroll', items)
    );
    run.later(() => listWatcher.scrollHandler(), 1000);
    this.set('listWatcher', listWatcher);
  },
  
  willDestroyElement() {
    try {
      this.get('listWatcher').destroy();
      this.get('transfersUpdater').destroy();
    } finally {
      this._super(...arguments);
    }
  },
  
  openedTransfersSlice: computed('activeTabId', function () {
    /** @type {string} */
    const activeTabId = this.get('activeTabId');
    return this.get(`${activeTabId}Transfers`);
  }),
  
  // forceTableTopChange: observer('openedTransfersSlice.[]', function () {
  //   console.log('xdebug: starindexchange');
  //   run.next(() => {
  //     this.get('listWatcher').scrollHandler();
  //   });
  // }),
  
  obs1: observer('openedTransfersSlice', function () {
    run.scheduleOnce('afterRender', () => {
      this.get('listWatcher').scrollHandler();
    });
  }),
  
  /**
   * 
   * @param {Array<HTMLElement>} items 
   */
  onTableScroll(items) {
    const {
      activeTabId,
      openedTransfersSlice,
      transfersUpdater,
    } = this.getProperties('activeTabId', 'openedTransfersSlice', 'transfersUpdater');
    /** @type {Array<string>} */
    const allTransferIds = this.get(`${activeTabId}TransferList.content`).hasMany('list').ids();
    /** @type {Array<string>} */
    const renderedTransferIds = items.map(i => i.id.match(/transfer-row-(.*)/)[1]);
    const firstId = renderedTransferIds[0];
    const lastId = renderedTransferIds[renderedTransferIds.length - 1];
    const startIndex = allTransferIds.indexOf(firstId);
    const endIndex = allTransferIds.indexOf(lastId);
    
    const oldVisibleIds = openedTransfersSlice.mapBy('id');
    openedTransfersSlice.setProperties({ startIndex, endIndex });
    const newVisibleIds = openedTransfersSlice.mapBy('id');
    transfersUpdater.set('visibleIds', newVisibleIds);
    
    transfersUpdater.fetchSpecificRecords(_.difference(newVisibleIds, oldVisibleIds));
    
    // FIXME: does not work
    run.next(() => {
      if (startIndex > 0 && get(openedTransfersSlice, 'firstObject.id') === firstId) {
        this.get('listWatcher').scrollHandler();
      }
    });
    
    
    // FIXME: debug code
    console.log(`visible: ${renderedTransferIds}`);
    console.log(`indexes: ${startIndex}..${endIndex}`);
  },
});
