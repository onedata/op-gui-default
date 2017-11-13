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
  run,
  RSVP: { Promise },
} = Ember;

// FIXME: 

const UPDATE_INTERVAL = 2 * 1000;

import Looper from 'ember-cli-onedata-common/utils/looper';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

// FIXME: update providers if there is provider referenced that is not on list

export default EmberObject.extend({
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
   * @type {number}
   */
  _sharedInterval: computed('isEnabled', function () {
    if (this.get('isEnabled')) {
      return UPDATE_INTERVAL;
    } else {
      return null;
    }
  }),

  /**
   * Interval [ms] used by `_currentWatcher`
   * @type {number}
   */
  _currentInterval: computed.reads('_sharedInterval'),
  
  _completedInterval: computed.reads('_sharedInterval'),

  init() {
    this._super(...arguments);

    this.setProperties({
      currentIsUpdating: true,
      completedIsUpdating: true,
    });

    this._createWatchers();
    this._reconfigureWatchers();

    // get properties to enable observers
    this.getProperties('_currentInterval', '_completedInterval');
  },

  destroy() {
    try {
      this.setProperties({
        _currentInterval: undefined,
        _completedInterval: undefined,
      });
      _.each(
        _.values(this.getProperties('_currentWatcher', '_completedWatcher')),
        watcher => watcher.destroy()
      );
    } finally {
      this._super(...arguments);
    }
  },

  _reconfigureWatchers: observer(
    '_currentInterval',
    '_completedInterval',
    function () {
      // debouncing does not let _setWatchersIntervals to be executed multiple
      // times, which can occur for observer
      run.debounce(this, '_setWatchersIntervals', 1);
    }
  ),

  // TODO: there should be no watcher for reports at all - it should be updated:
  // - after enabling
  // - on change status.inProgress true -> false

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
    // FIXME: too often
    _completedWatcher
      .on('tick', () =>
        safeExec(this, 'fetchCompleted')
      );
         
    this.setProperties({
      _currentWatcher,
      _completedWatcher,
    });
  },

  _setWatchersIntervals() {
    // this method is invoked from debounce, so it's "this" can be destroyed
    if (this.isDestroyed === false) {
      const {
        _currentInterval,
        _completedInterval,
        _currentWatcher,
        _completedWatcher,
      } = this.getProperties(
        '_currentInterval',
        '_completedInterval',
        '_currentWatcher',
        '_completedWatcher'
      );
      set(_currentWatcher, 'interval', _currentInterval);
      set(_completedWatcher, 'interval', _completedInterval);
    }
  },

  fetchCurrent() {
    return this.fetchList('current');
  },
  
  // FIXME: currentStat for completed transfers should be fetched/refreshed only once
  fetchCompleted() {
    return this.fetchList('completed');
  },
  
  fetchList(type) {
    const {
      space,
    } = this.getProperties('space');
    this.set(`${type}IsUpdating`, true);
    
    return space.belongsTo(`${type}TransferList`).reload()
      // .then(ctl => {
      //   console.debug('reloaded: ' + ctl);
      // })
      .then(transferList => transferList.get('list'))
      .then(list => Promise.all(list.map(t => t.belongsTo('currentStat').reload())))
      // .then(transfers => tranfer.belongsTo('currentStat').reload())
      .catch(error => this.set(`${type}Error`, error))
      .finally(() => this.set(`${type}IsUpdating`, false));
  },

});
