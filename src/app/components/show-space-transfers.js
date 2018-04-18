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
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import generateColors from 'op-worker-gui/utils/generate-colors';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import ArraySlice from 'ember-cli-onedata-common/utils/array-slice';
import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';
import ListWatcher from 'op-worker-gui/utils/list-watcher';

const {
  Component,
  computed,
  observer,
  isArray,
  get,
  A,
  isEmpty,
  inject: { service },
  run,
  RSVP: {
    Promise
  },
} = Ember;

const defaultActiveTabId = 'on-the-fly';

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
   * Ids of transfers that should be expanded, "blinked" and scrolled to
   * on entering view
   * @type {Array<string>|undefined}
   */
  selectedTransferIds: undefined,
  
  /**
   * @virtual
   * @type {function}
   */
  changeListTab: undefined,
  
  //#endregion
  
  //#region Internal properties
      
  listLocked: false,
  
  activeTabId: defaultActiveTabId,
  
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
      transfersUpdater: oldTransfersUpdater,
    } = this.getProperties(
      '_transfersUpdaterEnabled',
      'space',
      'store',
      'activeTabId',
      'transfersUpdater',
      // just enable observers
      'allTablesLoaded'
    );
    
    if (oldTransfersUpdater) {
      oldTransfersUpdater.destroy();
    }
    const transfersUpdater = SpaceTransfersUpdater.create({
      store,
      isEnabled: _transfersUpdaterEnabled,
      scheduledEnabled: activeTabId === 'scheduled',
      currentEnabled: activeTabId === 'current',
      completedEnabled: activeTabId === 'completed',
      space: space,
    });    
    this.set('transfersUpdater', transfersUpdater);
    
    this._initializeDefaultValues();
    
    return transfersUpdater;
  },
    
  /**
   * Assume that transfer lists (ids) are loaded
   * If one of selected transfer is found on the completed list - go to this list and scroll/expand
   * Otherwise, serch on current, and next search on scheduled
   */
  _scrollToFirstSelectedTransfer() {
    const selectedTransferIds = this.get('selectedTransferIds');
    const includedInSelectedTransfers = (id) => {
      return _.includes(selectedTransferIds, id);
    };
    
    let selectedList;
    let indexOnList;
    for (const transferType of ['completed', 'current', 'scheduled']) {
      const transferIds = this.get(transferType + 'TransferList.content').hasMany('list').ids();
      const index = _.findIndex(transferIds, includedInSelectedTransfers);
      if (index > -1) {
        selectedList = transferType;
        indexOnList = index;
        break;
      }
    }

    if (selectedList) {
      this.set('listLocked', true);
      this.set('activeTabId', selectedList);
      run.scheduleOnce('afterRender', this, function () {
        this.get('openedTransfersSlice').setProperties({
          startIndex: indexOnList,
          endIndex: indexOnList,
        });
        run.next(() => {
          const $tr = this.$(`tr.transfer-row[data-list-index=${indexOnList}]`);
          // magic number... after render, tr jumps to top, currently don't know why
          $('#content-scroll').scrollTop($tr.offset().top - 800);
          run.next(() => {
            this.set('listLocked', false);
          });
        });
      });
    }
  },
  
  _initializeDefaultValues() {
    this.set('_ptcCache', A());
  },
  
  observeScrollToSelectedTransfers: observer('selectedTransferIds', 'allTablesLoaded', function () {
    if (this.get('allTablesLoaded') && !this.get('_scrolledToSelectedTransfers')) {
      run.next(() => this._scrollToFirstSelectedTransfer());
      this.set('_scrolledToSelectedTransfers', true);
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
  
  /**
   * Updates transfers data needed by most of visible components.
   * Initialized in `init`.
   * @type {SpaceTransfersUpdater}
   */
  transfersUpdater: undefined,
  
  providerId: computed.reads('session.sessionDetails.providerId'),
  
  _transfersUpdaterEnabled: computed.readOnly('transfersUpdaterEnabled'),
  
  scheduledTransferList: computed.reads('space.scheduledTransferList'),
  currentTransferList: computed.reads('space.currentTransferList'),
  completedTransferList: computed.reads('space.completedTransferList'),
  onTheFlyTransferList: computed.reads('space.onTheFlyTransferList'),
  providerList: computed.reads('space.providerList'),
  providersMap: computed.reads('space.transferLinkState.activeLinks'),
  
  /**
   * Collection of Transfer model for on the fly transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  onTheFlyTransfers: computed.reads('onTheFlyTransferList.list.content'),

  /**
   * Collection of Transfer model for scheduled transfers
   * @type {Ember.ComputedProperty<ArraySlice<Transfer>>}
   */
  scheduledTransfers: undefined,
  
  /**
   * Collection of Transfer model for current transfers
   * @type {Ember.ComputedProperty<ArraySlice<Transfer>>}
   */
  currentTransfers: undefined,
  
  /**
   * Collection of Transfer model for completed transfers
   * @type {Ember.ComputedProperty<ArraySlice<Transfer>>}
   */
  completedTransfers: undefined,
    
  providersLoaded: computed.reads('providerList.queryList.isSettled'),
  providersError: computed.reads('providerList.queryList.reason'),

  scheduledTransfersLoaded: computed.reads('scheduledTransferList.isLoaded'),
  currentTransfersLoaded: computed.reads('currentTransferList.isLoaded'),
  completedTransfersLoaded: computed.reads('completedTransferList.isLoaded'),
  
  onTheFlyTransfersLoaded: computed(
    'onTheFlyTransferList.isLoaded',
    'onTheFlyTransfers.isLoaded',
    'onTheFlyTransfers.@each.isLoading',
    function getCurrentTransfersLoaded() {
      return this.get('onTheFlyTransferList.isLoaded') === true &&
        this.get('onTheFlyTransfers.isLoaded') === true &&
        this.get('onTheFlyTransfers').every(transfer => !transfer.get('isLoading'));
    }
  ),

  onTheFlyProviders: computed('onTheFlyTransfersLoaded', 'providersLoaded', function () {
    const {
      space,
      providersLoaded,
      providers,
      onTheFlyTransfers,
      onTheFlyTransfersLoaded,
      store,
    } = this.getProperties(
      'space',
      'providersLoaded',
      'providers',
      'onTheFlyTransfers',
      'onTheFlyTransfersLoaded',
      'store'
    );
    let promise;
    if (!providersLoaded || !onTheFlyTransfersLoaded) {
      promise = Promise.resolve(A());
    } else {
      const providersIds =
        onTheFlyTransfers.map(transfer => get(transfer, 'destination'));
      const providersIdsToLoad =
        _.difference(providersIds, providers.map(p => get(p, 'id')));
      if (providersIdsToLoad.length) {
        promise = Promise.all(providersIdsToLoad.map(providerId =>
          store.queryRecord('system-provider', {
            id: providerId,
            context: {
              od_space: get(space, 'id'),
            },
          })
        )).then((loaded) => A(loaded.concat(providers)));
      } else {
        promise = Promise.resolve(A(providers));
      }
    }
    return PromiseArray.create({
      promise,
    });
  }),

  onTheFlyTransfersProvidersLoaded: computed.and(
    'onTheFlyTransfersLoaded',
    'onTheFlyProviders.isFulfilled'
  ),

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  allTablesLoaded: computed.and(
    'isSupportedByCurrentProvider',
    'providersLoaded',
    'scheduledTransfersLoaded',
    'currentTransfersLoaded',
    'completedTransfersLoaded'
  ),
    
  /**
   * If true, this instance of data container already scrolled to selected transfers
   * @type {boolean}
   */
  _scrolledToSelectedTransfers: false,
  
  /**
   * Cache for `providerTransferConnections`
   * @type {Ember.Array<ProviderTransferConnection>}
   */
  _ptcCache: undefined,
  
  /**
   * Collection of connection between two providers (for map display)
   * Order in connection is random; each pair can occur once.
   * See `util:transfers/provider-transfer-connections`
   * `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
   * @type {Ember.ComputedProperty<Array<ProviderTransferConnection|undefined>>}
   */
  providerTransferConnections: computed(
    'providersMap',
    '_ptcCache',
    function getProviderTransferConnections() {
      const providersMap = this.get('providersMap');
      let _ptcCache = this.get('_ptcCache');
      if (providersMap) {
        mutateArray(
          _ptcCache,
          providerTransferConnections(providersMap),
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
    'providersMap',
    function getDestinationProviderIds() {
      const providersMap = this.get('providersMap');
      if (!isEmpty(providersMap)) {
        return _.uniq(_.flatten(_.values(providersMap)));
      }
    }
  ),
  
  /**
   * Creates an array of provider ids that are source of transfers for space
   * NOTE: returns new array every recomputation
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  sourceProviderIds: computed(
    'providersMap',
    function getSourceProviderIds() {
      const providersMap = this.get('providersMap');
      if (!isEmpty(providersMap)) {
        return Object.keys(providersMap);
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
      const colors = generateColors(providerIds.length + 1);
      return _.assign(
        _.zipObject(providerIds, colors),
        { 'unknown': colors[colors.length - 1] }
      );
    }
  }),
  
  //#endregion
  
  tabChanged: observer('activeTabId', function observeTabChanged() {
    this.get('changeListTab')(this.get('activeTabId'));
  }),
  
  enableWatcherCollection: observer('activeTabId', function enableWatcherCollection() {
    const {
      activeTabId,
      transfersUpdater,
    } = this.getProperties('activeTabId', 'transfersUpdater');    
    transfersUpdater.setProperties({
      scheduledEnabled: activeTabId === 'scheduled',
      currentEnabled: activeTabId === 'current',
      completedEnabled: activeTabId === 'completed',
    });
  }),
  
  scheduledTransfersLoadingMore: false,
  currentTransfersLoadingMore: false,
  completedTransfersLoadingMore: false,
  
  spaceChanged: observer('space', function observeSpaceChanged() {
    this.reinitializeTransfers();
  }),
  
  init() {
    this._super(...arguments);
    this.spaceChanged();
    this.observeScrollToSelectedTransfers();
  },
  
  reinitializeTransfers() {
    const transfersUpdater = this._initTransfersData();
    ['scheduled', 'current', 'completed'].forEach(type => {
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
    const listWatcher = this.get('listWatcher');
    if (listWatcher) {
      listWatcher.scrollHandler();
    }
    this.setProperties({
      listLocked: false,
    });
  },
  
  didInsertElement() {
    const listWatcher = new ListWatcher(
      $('#content-scroll'),
      '.transfer-row',
      items => safeExec(this, 'onTableScroll', items)
    );
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

  openedTransfersSlice: computed(
    'scheduledTransfers',
    'currentTransfers',
    'completedTransfers',
    'activeTabId',
    function () {
      /** @type {string} */
      const activeTabId = this.get('activeTabId');
      return activeTabId !== 'on-the-fly' ? this.get(`${activeTabId}Transfers`) : null;
    }
  ),
  
  /**
   * 
   * @param {Array<HTMLElement>} items 
   */
  onTableScroll(items) {
    const {
      activeTabId,
      openedTransfersSlice,
      transfersUpdater,
      listLocked,
    } = this.getProperties('activeTabId', 'openedTransfersSlice', 'transfersUpdater', 'listLocked');
    if (!listLocked && activeTabId !== 'on-the-fly') {
      /** @type {Array<string>} */
      const allTransferIds = this.get(`${activeTabId}TransferList.content`).hasMany('list').ids();
      /** @type {Array<string>} */
      const renderedTransferIds = items.map(i => i.getAttribute('data-transfer-id'));
      const firstId = renderedTransferIds[0];
      const lastId = renderedTransferIds[renderedTransferIds.length - 1];
      const startIndex = allTransferIds.indexOf(firstId);
      const endIndex = allTransferIds.indexOf(lastId, startIndex);

      const oldVisibleIds = openedTransfersSlice.mapBy('id');
      openedTransfersSlice.setProperties({ startIndex, endIndex });
      const newVisibleIds = openedTransfersSlice.mapBy('id');
      transfersUpdater.set('visibleIds', newVisibleIds);

      transfersUpdater.fetchSpecificRecords(_.difference(newVisibleIds, oldVisibleIds));

      run.next(() => {
        if (startIndex > 0 && get(openedTransfersSlice, 'firstObject.id') === firstId) {
          this.get('listWatcher').scrollHandler();
        }
      });

      const isLoadingMore = (
        get(openedTransfersSlice, 'lastObject') !==
        get(openedTransfersSlice, 'sourceArray.lastObject')
      );
      this.set(`${activeTabId}TransfersLoadingMore`, isLoadingMore);
    }
  },
});
