/**
 * A view component for onedata.transfers.show route
 *
 * @module components/show-space-transfers
 * @author Jakub Liput
 * @copyright (C) 2017-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import generateColors from 'op-worker-gui/utils/generate-colors';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';
import ListWatcher from 'op-worker-gui/utils/list-watcher';

const {
  Component,
  computed,
  computed: {
    reads,
  },
  observer,
  isArray,
  get,
  A,
  isEmpty,
  inject: { service },
  RSVP: {
    Promise
  },
  run: {
    next,
  },
} = Ember;

export default Component.extend({
  classNames: ['show-space-transfers', 'row'],
  session: service(),
  store: service(),
  oneproviderServer: service(),
  
  //#region External properties
  
  /**
   * Space model, which transfers will be listed
   * @virtual
   * @type {Space}
   */
  space: undefined,
    
  /**
   * @virtual
   * @type {function}
   */
  changeListTab: undefined,
  
  /**
   * @virtual optional
   * If set, skip auto select of first opened tab and use injected tab ID
   * @type {string|null}
   */
  defaultTab: undefined,
  
  /**
   * An ID of file for which a special transfers tab will be created.
   * If undefined/null the tab will not be created
   * @type {string|undefined}
   */
  fileId: undefined,
  
  //#endregion
  
  //#region Internal properties
      
  listLocked: false,
  
  activeTabId: computed.reads('initialTab.content'),
  
  /**
   * Holds tab ID that was opened recently.
   * It should be cleared if some operations after opening has been done.
   * @type {string|null}
   */
  _tabJustChangedId: null,
  
  /**
   * @type {boolean}
   */
  _isTransfersTableBegin: undefined,
  
  //#endregion
  
  //#region Computed properties

  /**
   * A file record for which a special tab will be rendered.
   * If no `fileId` is provided - undefined.
   * If file is broken - rejects.
   * @type {Ember.ComputedProperty<PromiseObject<models.File>>|undefined}
   */
  fileProxy: computed('fileId', function () {
    const {
      store,
      fileId,
    } = this.getProperties('store', 'fileId');
    if (fileId) {
      const promise = store.findRecord('file', fileId)
        .then(record => {
          if (get(record, 'type') === 'broken') {
            throw { message: 'not_found' };
          } else {
            return record;
          }
        });
      return PromiseObject.create({ promise });
    }
  }),

  /**
   * @type {Ember.ComputedProperty<models.File>}
   */
  file: reads('fileProxy.content'),
  
  /**
   * Name of icon to use in file tab
   * @type {Ember.ComputedProperty<string>}
   */
  _fileTabIcon: computed('file.isDir', function () {
    return this.get('file.isDir') ? 'folder' : 'file';
  }),
  
  /**
   * List of provider IDs of the opened on-the-fly transfers charts.
   * Should be updated by the `on-the-fly-list` component.
   * Note: all provider IDs will be opened if providers list will be modified,
   * but for now the provider list is constant.
   * @type {Ember.Array<string>}
   */
  _onTheFlyOpenedProviderIds: computed('providers.@each.id', function () {
    return A(this.get('providers').map(p => get(p, 'id')));
  }),
        
  /**
   * Max number of ended transfers that can be fetched for transfer
   * @type {Ember.ComputedProperty<number>}
   */
  _historyLimitPerFile: computed.reads('session.sessionDetails.config.transfersHistoryLimitPerFile'),
  
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
  isSupportedByCurrentProvider: computed('providerId', 'providers.[]', function () {
    const {
      providers,
      providerId,
    } = this.getProperties('providerId', 'providers');
    if (isArray(providers) && providerId != null) {
      return _.includes(providers.map(p => get(p, 'id')), providerId);
    } else {
      return null;
    }
  }),
  
  //#endregion
      
  //#region Feature: transfers data container
    
  /**
   * Number of loaded ended transfers for file tab.
   * @type {Ember.ComputedProperty<number>}
   */
  _fileEndedTransfersCount: computed(
    'fileTransfers.sourceArray.@each.finishTime',
    function () {
      const allFileTransfers = this.get('fileTransfers.sourceArray');
      if (allFileTransfers) {
        return this.get('fileTransfers.sourceArray')
        .reduce(
          (sum, transfer) => sum + (get(transfer, 'finishTime') ? 1 : 0),
          0
        );
      }
    }),
  
  /**
   * True if the `_endedTransfersCount` reached history limit
   * @type {boolean}
   */
  _fileHistoryLimitReached: computed('fileTransfersLoadingMore', '_historyLimitPerFile', '_fileEndedTransfersCount', function () {
    if (!this.get('fileTransfersLoadingMore')) {
      const {
        _historyLimitPerFile,
        _fileEndedTransfersCount,
      } = this.getProperties('_historyLimitPerFile', '_fileEndedTransfersCount');
      return _fileEndedTransfersCount >= _historyLimitPerFile;
    }
  }),
  
  activeListUpdaterId: computed('activeTabId', '_isTransfersTableBegin', function () {
    if (this.get('_isTransfersTableBegin')) {
      return this.get('activeTabId');
    }
  }),
  
  transfersUpdaterEnabled: true,
  
  _initTransfersData() {
    const {
      _transfersUpdaterEnabled,
      space,
      store,
      transfersUpdater: oldTransfersUpdater,
      activeListUpdaterId,
      activeTabId,
      fileProxy,
      file,
    } = this.getProperties(
      '_transfersUpdaterEnabled',
      'space',
      'store',
      'activeTabId',
      'activeListUpdaterId',
      'transfersUpdater',
      'fileProxy',
      'file'
    );
    
    if (oldTransfersUpdater) {
      oldTransfersUpdater.destroy();
    }
    const transfersUpdater = SpaceTransfersUpdater.create({
      store,
      isEnabled: _transfersUpdaterEnabled,
      scheduledEnabled: activeListUpdaterId === 'scheduled',
      currentEnabled: activeListUpdaterId === 'current',
      currentStatEnabled: activeTabId === 'scheduled' || activeTabId === 'current',
      completedEnabled: activeListUpdaterId === 'completed',
      fileEnabled: activeTabId === 'file' && fileProxy && get(fileProxy, 'isFulfilled'),
      space: space,
      file: file,
    });    
    this.set('transfersUpdater', transfersUpdater);
    
    this._initializeDefaultValues();
    
    return transfersUpdater;
  },
    
  _initializeDefaultValues() {
    this.set('_ptcCache', A());
  },
  
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
  
  /**
   * @type {Ember.ComputedProperty<FakeListRecordRelation|undefined>}
   */
  fileTransferList: computed.reads('file.transferList'),
  
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
   * @type {Ember.ComputedProperty<ReplacingChunksArray<Transfer>>}
   */
  scheduledTransfers: undefined,
  
  /**
   * Collection of Transfer model for current transfers
   * @type {Ember.ComputedProperty<ReplacingChunksArray<Transfer>>}
   */
  currentTransfers: undefined,
  
  /**
   * Collection of Transfer model for completed transfers
   * @type {Ember.ComputedProperty<ReplacingChunksArray<Transfer>>}
   */
  completedTransfers: undefined,

  /**
   * Collection of Transfer model for file transfers
   * @type {Ember.ComputedProperty<ReplacingChunksArray<Transfer>>}
   */
  fileTransfers: undefined,
  
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
        onTheFlyTransfers.map(transfer => get(transfer, 'replicatingProvider'));
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
  
  /**
   * @type {Ember.ComputedProperty<PromiseObject<string>>}
   */
  initialTab: computed(function () {
    const defaultTab = this.get('defaultTab');
    const fileId = this.get('fileId');
    if (defaultTab &&
      (defaultTab === 'file' && fileId) || _.includes(['scheduled', 'current', 'complete'], defaultTab)
    ) {
      return PromiseObject.create({ promise: Promise.resolve(defaultTab) });
    } else if (fileId) {
      return PromiseObject.create({ promise: Promise.resolve('file') });
    } else {
      const promise = Promise.all(
        ['scheduled', 'current', 'completed'].map(transferType =>
          this.get(transferType + 'TransferList')
        )
      ).then(([ scheduledList, currentList, completedList ]) => {
        if (get(scheduledList, 'length') > 0) {
          return 'scheduled';
        } else if (get(currentList, 'length') > 0) {
          return 'current';
        } else if (get(completedList, 'length') > 0) {
          return 'completed';
        } else {
          return 'on-the-fly';
        }
      });
      return PromiseObject.create({ promise }); 
    }
  }),
  
  tabChanged: observer('activeTabId', function observeTabChanged() {
    const activeTabId = this.get('activeTabId');
    this.get('changeListTab')(activeTabId);
    this.set('_tabJustChangedId', activeTabId);
  }),
  
  enableWatcherCollection: observer(
    'activeListUpdaterId',
    'activeTabId',
    'file',
    'fileProxy.isFulfilled',
    function enableWatcherCollection() {
      const {
        activeListUpdaterId,
        activeTabId,
        transfersUpdater,
        file,
        fileProxy,
      } = this.getProperties(
        'activeListUpdaterId', 
        'transfersUpdater', 
        'activeTabId',
        'file',
        'fileProxy'
      );
      transfersUpdater.setProperties({
        scheduledEnabled: activeListUpdaterId === 'scheduled',
        currentEnabled: activeListUpdaterId === 'current',
        currentStatEnabled: _.includes(['scheduled', 'current', 'file'], activeTabId),
        completedEnabled: activeListUpdaterId === 'completed',
        fileEnabled: activeTabId === 'file' && fileProxy && get(fileProxy, 'isFulfilled'),
        file,
      });
    }),
    
  scheduledTransfersLoadingMore: false,
  currentTransfersLoadingMore: false,
  completedTransfersLoadingMore: false,
  fileTransfersLoadingMore: false,
  
  fileChanged: observer('fileProxy.content', function observerFileChanged() {
    if (this.get('file')) {
      this.initTransfers('file');
    }
  }),
  
  spaceChanged: observer('space', function observeSpaceChanged() {
    this._spaceChanged();
  }),
  
  init() {
    this._super(...arguments);
    this._spaceChanged(true);
    this.fileChanged();
  },

  _spaceChanged(isInit = false) {
    if (!isInit) {
      this._clearFileId();
    }
    this.reinitializeTransfers();
  },
  
  _clearFileId() {
    return this.sendAction('closeFileTab');
  },
  
  reinitializeTransfers() {
    this._initTransfersData();
    ['scheduled', 'current', 'completed'].forEach(type => {
      this.initTransfers(type);
    });
    const listWatcher = this.get('listWatcher');
    if (listWatcher) {
      listWatcher.scrollHandler();
    }
    this.setProperties({
      listLocked: false,
    });
  },
  
  initTransfers(type) {
    this.get(`${type}TransferList`).then(listRecord => {
      get(listRecord, 'list').then(list => {
        safeExec(this, 'set', `${type}Transfers`, list);
      });
      const visibleIds = listRecord.hasMany('list').ids();
      this.get('transfersUpdater').fetchSpecificRecords(visibleIds);
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

  openedTransfersChunksArray: computed(
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
   * @param {Array<HTMLElement>} items 
   */
  onTableScroll(items) {
    const {
      activeTabId,
      openedTransfersChunksArray,
      transfersUpdater,
      listLocked,
    } = this.getProperties('activeTabId', 'openedTransfersChunksArray', 'transfersUpdater', 'listLocked');
    if (!listLocked && activeTabId !== 'on-the-fly') {
      /** @type {Array<string>} */
      const transferListContent = this.get(`${activeTabId}TransferList.content`);
      if (!transferListContent) {
        return;
      }
      const allTransferIds = transferListContent.hasMany('list').ids();
      /** @type {Array<string>} */
      const renderedTransferIds = items.map(i => i.getAttribute('data-transfer-id'));
      const firstId = renderedTransferIds[0];
      const lastId = renderedTransferIds[renderedTransferIds.length - 1];
      const startIndex = allTransferIds.indexOf(firstId);
      const endIndex = allTransferIds.indexOf(lastId, startIndex);

      const oldVisibleIds = openedTransfersChunksArray.mapBy('id');
      openedTransfersChunksArray.setProperties({ startIndex, endIndex });
      const newVisibleIds = openedTransfersChunksArray.mapBy('id');
      transfersUpdater.set('visibleIds', newVisibleIds);

      transfersUpdater.fetchSpecificRecords(_.difference(newVisibleIds, oldVisibleIds));
      
      next(() => {
        if (startIndex > 0 && get(openedTransfersChunksArray, 'firstObject.id') === firstId) {
          this.get('listWatcher').scrollHandler();
        } else {
          this.set('_isTransfersTableBegin', startIndex <= 0);
        }
      });

      const isLoadingMore = (
        get(openedTransfersChunksArray, 'lastObject') !==
        get(openedTransfersChunksArray, 'sourceArray.lastObject')
      );
      this.set(`${activeTabId}TransfersLoadingMore`, isLoadingMore);
    }
  },

  actions: {
    /**
     * Start transfer cancel procedure
     * @param {string} transferId
     * @returns {Promise<undefined|any>}
     */
    cancelTransfer(transferId) {
      return this.get('oneproviderServer').cancelTransfer(transferId);
    },

    /**
     * Rerun transfer procedure
     * @param {string} transferId
     * @returns {Promise<undefined|any>}
     */
    rerunTransfer(transferId) {
      return this.get('oneproviderServer').rerunTransfer(transferId);
    },
    
    transferListChanged(/* type */) {
      const listWatcher = this.get('listWatcher');
      if (listWatcher) {
        listWatcher.scrollHandler();
      }
    },
    clearJustChangedTabId(type) {
      if (this.get('_tabJustChangedId') === type) {
        this.set('_tabJustChangedId', null);
      }
    },
    toggleOnTheFlyProviderId(providerId, opened) {
      const _onTheFlyOpenedProviderIds = this.get('_onTheFlyOpenedProviderIds');
      if (opened) {
        if (!_onTheFlyOpenedProviderIds.includes(providerId)) {
          _onTheFlyOpenedProviderIds.pushObject(providerId);
        }
      } else {
        _onTheFlyOpenedProviderIds.removeObject(providerId);
      }
    },
    closeFileTab() {
      this.set('activeTabId', 'scheduled');
      this._clearFileId();
    },
  },
});
