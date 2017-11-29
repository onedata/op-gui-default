/**
 * FIXME: doc
 *
 * @module utils/space-transfers-updater
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

const {
  Object: EmberObject,
  set,
  observer,
  computed,
  RSVP: { Promise },
} = Ember;

// FIXME: 

const UPDATE_CURRENT_INTERVAL = 2 * 1000;
const UPDATE_COMPLETED_INTERVAL = 10 * 1000;

import Looper from 'ember-cli-onedata-common/utils/looper';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

// FIXME: update providers if there is provider referenced that is not on list

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

  /**
   * Initialized with `_createWatchers`.
   * Updates info about current transfers:
   * - space.currentTransferList
   *   - for each transfer: transfer.currentStat
   * @type {Looper}
   */
  _currentWatcher: undefined,

  /**
   * FIXME: watcher for completed transfers
   */
  _completedWatcher: undefined,
  
  /**
   * If true, currently fetching info about current transfers
   * Set by some interval watcher
   * @type {boolean}
   */
  currentIsUpdating: undefined,

  /**
   * FIXME: implement
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
   * Error object from fetching completed transfers info
   * @type {any} typically a request error object
   */
  completedError: null,
  
  /**
   * Interval [ms] used by `_currentWatcher`
   * @type {number}
   */
  _currentInterval: computed.reads('_sharedInterval'),
  
  _completedInterval: computed.reads('_sharedInterval'),

  init() {
    this._super(...arguments);

    this.setProperties({
      currentIsUpdating: false,
      completedIsUpdating: false,
    });

    this._createWatchers();
    this._toggleWatchers();
    
    // get properties to enable observers
    this.get('_currentInterval');
    
    // FIXME: fill this when fist version of completed ids will be avail
    this.set('_completedIdsCache', []);
    this.set('_currentIdsCache', []);
  },

  destroy() {
    try {
      this.setProperties({
        _currentInterval: undefined,
        _completedInterval: undefined,
      });
      _.each(
        _.values(this.getProperties('_currentWatcher', '_completedWatcher')),
        watcher => watcher && watcher.destroy()
      );
    } finally {
      this._super(...arguments);
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
      
    this.setProperties({
      _currentWatcher,
      _completedWatcher,
    });
  },

  _toggleWatchers: observer('isEnabled', function () {
    // this method is invoked from debounce, so it's "this" can be destroyed
    if (this.isDestroyed === false) {
      const {
        isEnabled,
        _currentWatcher,
        _completedWatcher,
      } = this.getProperties(
        'isEnabled',
        '_currentWatcher',
        '_completedWatcher'
      );
      
      set(_currentWatcher, 'interval', isEnabled ? UPDATE_CURRENT_INTERVAL : null);
      set(_completedWatcher, 'interval', isEnabled ? UPDATE_COMPLETED_INTERVAL : null);
    }
  }),

  fetchCurrent() {
    console.debug('util:space-transfers-updater: fetchCurrent');
    
    const space = this.get('space');
    this.set('currentIsUpdating', true);
    const _currentIdsCache = this.get('_currentIdsCache');
    
    return space.belongsTo(`currentTransferList`).reload()
      .then(transferList => {        
        const currentIdsNew = transferList.hasMany('list').ids();
        const removedIds = _.difference(
          _currentIdsCache,
          currentIdsNew
        );
        this.set('_currentIdsCache', currentIdsNew);
        if (!_.isEmpty(removedIds)) {
          this.fetchCompleted();
        }
        return transferList.get('list');
      })
      // does not need to update transfer record as for active transfers it
      // changes only status from scheduled to active (we do not present it)
      .then(list => Promise.all(list.map(t => t.belongsTo('currentStat').reload())))
      .catch(error => this.set('currentError', error))
      .finally(() => this.set('currentIsUpdating', false));
  },
  
  /**
   * Should be invoked when:
   * - array of current transfers changes
   */
  fetchCompleted() {
    console.debug('util:space-transfers-updater: fetchCompleted invoked');
    
    if (this.get('completedIsUpdating') !== true) {
      console.debug('util:space-transfers-updater: fetchCompleted started');
      const store = this.get('store');
      const space = this.get('space');
              
      this.set(`completedIsUpdating`, true);
      
      const _completedIdsCache = this.get('_completedIdsCache');
      let newIds = [];
    
      return space.belongsTo(`completedTransferList`).reload()
        .then(transferList => {
          const completedIdsNew = transferList.hasMany('list').ids();
          newIds = _.difference(
            completedIdsNew,
            _completedIdsCache
          );
          this.set('_completedIdsCache', completedIdsNew);
          newIds.forEach(id => {
            const transfer = store.peekRecord('transfer', id);
            if (transfer && transfer.get('isOngoing')) {
              transfer.set('_completedReloading', true);
            }
          });
          return Promise.all(
            newIds.map(id => store.findRecord('transfer', id, { reload: true }))
          );
        })
        .then(transfers => {
          return Promise.all(
            transfers.map(t => t.belongsTo('currentStat').reload())
          );
        })
        .then(() => {
          newIds.forEach(id => {
            const transfer = store.peekRecord('transfer', id);
            if (transfer) {
              transfer.set('_completedReloading', undefined);
            }
          });
        })
        .catch(error => this.set(`completedError`, error))
        .finally(() => this.set(`completedIsUpdating`, false));
    } else {
      console.debug('util:space-transfers-updater: fetchCompleted skipped');
    }
  },

});
