/**
 * Updates transfers data for single space (except time statistics) by polling
 * 
 * Optionally update:
 * - collection of current transfers records with their current stats
 * - collection of completed transfers records
 * - handle updates of current stats when moving transfer from current to done
 *
 * @module utils/space-transfers-updater
 * @author Jakub Liput
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';
import ENV from 'op-worker-gui/config/environment';

const {
  Object: EmberObject,
  get,
  set,
  observer,
  computed,
  RSVP: { Promise },
  run: { later, debounce, next },
} = Ember;

/** 
 * How many milliseconds to wait between polling for single transfer data
 * @type {number}
 */
const TRANSFER_COLLECTION_DELAY = 300;

const DEFAULT_COMPLETED_TIME = 30 * 1000;
const MAP_TIME = 5000;

import Looper from 'ember-cli-onedata-common/utils/looper';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

// TODO: (low) update providers if there is provider referenced that is not on
// list; this can help with dynamically added providers

export default EmberObject.extend({
  /**
   * @virtual
   * Store service
   * @type {Ember.Service}
   */
  store: undefined,

  /**
   * @virtual
   * The model of space, will be used to perform fetching
   * @type {Space}
   */
  space: undefined,

  /**
   * After init, update is disabled by default
   * @virtual
   * @type {boolean}
   */
  isEnabled: false,

  _currentTransfersCount: 0,

  /**
   * Minimum time for polling (if there are no transfers)
   * @type {Ember.Computed<number>}
   */
  basePollingTime: 3 * 1000,

  /**
   * Only transfers with these ids will be updated
   * @type {Array<string>}
   */
  visibleIds: Object.freeze([]),
  
  /**
   * Polling interval (ms) used for fetching current transfers
   * @type {number}
   */
  pollingTimeCurrent: computed(
    '_currentTransfersCount',
    'basePollingTime',
    function getPollingTimeCurrent() {
      const currentTransfersCount = this.get('_currentTransfersCount');
      return currentTransfersCount ? this.computeCurrentPollingTime(currentTransfersCount) :
        this.get('basePollingTime');
    }
  ),

  /**
   * Polling interval (ms) used for fetching completed transfers
   * @type {number}
   */
  pollingTimeCompleted: DEFAULT_COMPLETED_TIME,

  /**
   * Polling interval (ms) used for fetching transfers map
   * @type {number}
   */
  pollingTimeMap: MAP_TIME,

  /**
   * @type {boolean}
   */
  currentEnabled: true,

  /**
   * @type {boolean}
   */
  completedEnabled: true,

  /**
   * @type {boolean}
   */
  mapEnabled: true,

  _currentEnabled: computed('currentEnabled', 'isEnabled', function () {
    return this.get('isEnabled') && this.get('currentEnabled');
  }),

  _completedEnabled: computed('completedEnabled', 'isEnabled', function () {
    return this.get('isEnabled') && this.get('completedEnabled');
  }),

  _mapEnabled: computed('mapEnabled', 'isEnabled', function () {
    return this.get('isEnabled') && this.get('mapEnabled');
  }),

  /**
   * Initialized with `_createWatchers`.
   * Updates info about current transfers:
   * - space.currentTransferList
   *   - for each transfer: transfer.currentStat
   * @type {Looper}
   */
  _currentWatcher: undefined,

  /**
   * @type {Looper}
   */
  _completedWatcher: undefined,

  /**
   * @type {Looper}
   */
  _mapWatcher: undefined,

  /**
   * If true, currently fetching info about current transfers
   * Set by some interval watcher
   * @type {boolean}
   */
  currentIsUpdating: undefined,

  /**
   * If true, currently fetching info about providers transfer mapping
   * Set by some interval watcher
   * @type {boolean}
   */
  mapIsUpdating: undefined,

  /**
   * If true, currently fetching info about completed transfers
   * Set by some interval watcher
   * @type {boolean}
   */
  completedIsUpdating: undefined,

  /**
   * Error object from fetching current transfers info
   * @type {any} typically a request error object
   */
  currentError: null,

  /**
   * Error object from fetching providers transfer mapping info
   * @type {any} typically a request error object
   */
  mapError: null,

  /**
   * Error object from fetching completed transfers info
   * @type {any} typically a request error object
   */
  completedError: null,

  /**
   * How much time [ms] to debounce when some property changes that
   * can occur watchers reconfiguration.
   * Set it to 0 for tests purposes.
   * @type {number}
   */
  _toggleWatchersDelay: ENV.environment === 'test' ? 0 : 1000,

  init() {
    this._super(...arguments);

    this.setProperties({
      currentIsUpdating: false,
      completedIsUpdating: false,
      mapIsUpdating: false,
    });

    this._createWatchers();
    this._toggleWatchers();

    // enable observers for properties
    this.getProperties('_currentEnabled', '_completedEnabled', '_mapEnabled');

    this.set('_completedIdsCache', []);
    this.set('_currentIdsCache', []);

    next(() => safeExec(this, 'countCurrentTransfers'));
  },

  destroy() {
    try {
      _.each(
        _.values(
          this.getProperties('_currentWatcher', '_completedWatcher', '_mapWatcher')
        ),
        watcher => watcher && watcher.destroy()
      );
    } finally {
      this._super(...arguments);
    }
  },

  /**
   * Updates `_currentTransfersCount` property
   */
  countCurrentTransfers() {
    const newCount = this.get('space.currentTransferList.content').hasMany('list').ids()
      .length;
    if (newCount !== this.get('_currentTransfersCount')) {
      this.set('_currentTransfersCount', newCount);
    }
  },
  
  /**
   * Create watchers for fetching information
   */
  _createWatchers() {
    const _currentWatcher = Looper.create({
      immediate: true,
    });
    _currentWatcher
      .on('tick', () => 
          safeExec(this, 'fetchCurrent')
      );

    const _completedWatcher = Looper.create({
      immediate: true,
    });
    _completedWatcher
      .on('tick', () =>
        safeExec(this, 'fetchCompleted')
      );
    
    const _mapWatcher = Looper.create({
      immediate: true,
    });
    _mapWatcher
      .on('tick', () =>
        safeExec(this, 'fetchProviderMap')
      );

    this.setProperties({
      _currentWatcher,
      _completedWatcher,
      _mapWatcher,
    });
  },

  observeToggleWatchers: observer(
    '_currentEnabled',
    '_completedEnabled',
    '_mapEnabled',
    'pollingTimeCurrent',
    'pollingTimeCompleted',
    'pollingTimeMap',
    '_toggleWatchersDelay',
    function () {
      debounce(this, '_toggleWatchers', this.get('_toggleWatchersDelay'));
    }),

  _toggleWatchers() {
    // this method is invoked from debounce, so it's "this" can be destroyed
    safeExec(this, () => {
      const {
        _currentEnabled,
        _completedEnabled,
        _mapEnabled,
        _currentWatcher,
        _completedWatcher,
        _mapWatcher,
        pollingTimeCurrent,
        pollingTimeCompleted,
        pollingTimeMap,
      } = this.getProperties(
        '_currentEnabled',
        '_completedEnabled',
        '_mapEnabled',
        '_currentWatcher',
        '_completedWatcher',
        '_mapWatcher',
        'pollingTimeCurrent',
        'pollingTimeCompleted',
        'pollingTimeMap'
      );

      set(
        _currentWatcher,
        'interval',
        _currentEnabled ? pollingTimeCurrent : null
      );
      set(
        _completedWatcher,
        'interval',
        _completedEnabled ? pollingTimeCompleted : null
      );
      set(
        _mapWatcher,
        'interval',
        _mapEnabled ? pollingTimeMap : null
      );
    });
  },

  fetchSpecificRecords(ids, reload = false) {
    const store = this.get('store');
    return Promise.all(ids.map(id =>
      store.findRecord('transfer', id, { reload })
        .then(transfer => {
          if (reload) {
            return transfer.belongsTo('currentStat').reload()
              .then(() => transfer);
          } else {
            return transfer;
          }
        })
    ));
  },
  
  /**
   * Function invoked when current transfers should be updated by polling timer
   * @return {Promise<Array<TransferCurrentStat>>} resolves with current stats
   *    of updated current transfers
   */
  fetchCurrent(immediate = false) {
    // FIXME: use ids (new ids to fetch)
    console.debug('FIXME: xdebug util:space-transfers-updater: fetchCurrent started');
    const space = this.get('space');
    this.set('currentIsUpdating', true);
    const visibleIds = this.get('visibleIds');
    const _currentIdsCache = this.get('_currentIdsCache');

    return space.belongsTo(`currentTransferList`).reload()
      .then(transferList => safeExec(this, () => {
        this.countCurrentTransfers();
        const currentIdsNew = transferList.hasMany('list').ids();
        const removedIds = _.difference(
          _currentIdsCache,
          currentIdsNew
        );
        // TODO: not very safe - _currentIdsCache could be changed before async
        // fetchCompleted will be invoked
        this.set('_currentIdsCache', currentIdsNew);
        // FIXME: this auto-update can be replaced with fetching when changing tab
        if (!_.isEmpty(removedIds)) {
          later(() => {
            this.fetchSpecificRecords(removedIds);
          }, 5000);
        }
        return transferList;
      }))
      .then(() => this.fetchSpecificRecords(visibleIds, true))
      // does not need to update transfer record as for active transfers it
      // changes only status from scheduled to active (we do not present it)
      .then(list => safeExec(this, () => {
        const transfersCount = get(list, 'length');
        if (immediate) {
          return Promise.all(list.map(transfer =>
            transfer.belongsTo('currentStat').reload()
          ));
        } else {
          return Promise.all(
            list.map((transfer, i) =>
              safeExec(
                this,
                '_reloadTransferCurrentStat',
                transfer,
                i,
                transfersCount
              )
            ));
        }
        
      }))
      .catch(error => safeExec(this, () => this.set('currentError', error)))
      .finally(() => safeExec(this, () => this.set('currentIsUpdating', false)));
  },

  _reloadTransferCurrentStat(transfer, index, transfersCount) {
    const pollingTimeCurrent = this.get('pollingTimeCurrent');
    const delay = (pollingTimeCurrent * index) / transfersCount;
    return new Promise((resolve, reject) => {
      later(
        () => {
          // checking if updater is still in use
          if (!this.isDestroyed) {
            transfer.belongsTo('currentStat').reload()
            .then(resolve)
            .catch(reject); 
          }
        },
        delay
      );
    });
  },

  computeCurrentPollingTime(transfersCount) {
    return TRANSFER_COLLECTION_DELAY * transfersCount + this.get('basePollingTime');
  },

  /**
   * Function invoked when providers transfer mapping should be updated by
   * polling timer
   * @return {Promise<SpaceTransferLinkState>}
   */
  fetchProviderMap() {
    this.set('mapIsUpdating', true);
    return this.get('space').belongsTo(`transferLinkState`).reload()
      .catch(error => safeExec(this, () => this.set('mapError', error)))
      .finally(() => safeExec(this, () => this.set('mapIsUpdating', false)));
  },
  
  /**
   * Should be invoked when:
   * - array of current transfers changes
   * @returns {Promise<Array<Transfer>>} transfers that was added to completed list
   */
  fetchCompleted() {
    if (this.get('completedIsUpdating') !== true) {
      console.debug('FIXME: xdebug util:space-transfers-updater: fetchCompleted started');
      const {
        space,
      } = this.getProperties('space');
      
      this.set(`completedIsUpdating`, true);
      // const visibleIds = this.get('visibleIds');

      return space.belongsTo(`completedTransferList`).reload()
        // .then(() => this.fetchSpecificRecords(visibleIds))
        .then(modifiedTransfers => {
          return Promise.all(
            modifiedTransfers.map(t => t.belongsTo('currentStat').reload())
          );
        })
        .catch(error => safeExec(this, () => this.set(`completedError`, error)))
        .finally(() => safeExec(this, () => this.set(`completedIsUpdating`, false)));
    } else {
      console.debug('util:space-transfers-updater: fetchCompleted skipped');
    }
  },

});
