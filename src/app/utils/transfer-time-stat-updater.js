/**
 * FIXME: doc
 *
 * @module utils/transfer-time-stat-updater
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
} = Ember;

// FIXME: 

const UPDATE_INTERVAL = 2 * 1000;

import Looper from 'ember-cli-onedata-common/utils/looper';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

// FIXME: update providers if there is provider referenced that is not on list

export default EmberObject.extend({
  /**
   * @virtual
   * The model of TransferTimeStat that should be updated periodically
   * @type {TransferTimeStat}
   */
  timeStat: undefined,

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
        _.values(this.getProperties('_currentInterval', '_completedInterval')),
        watcher => watcher.destroy()
      );
    } finally {
      this._super(...arguments);
    }
  },

  _reconfigureCleanWatchers: observer(
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
    const _cleanStatusWatcher = Looper.create({
      immediate: true,
    });
    _cleanStatusWatcher
      .on('tick', () =>
        safeExec(this, 'fetchCleanStatus')
      );

    const _cleanReportsWatcher = Looper.create({
      immediate: true,
    });
    _cleanReportsWatcher
      .on('tick', () =>
        safeExec(this, 'fetchCleanReports')
      );

    this.setProperties({
      _cleanReportsWatcher,
      _cleanStatusWatcher,
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
    const {
      space,
    } = this.getProperties('space');
    this.set('currentIsUpdating', true);
    
    return space.get('currentTransferList')
      .then(ctl => ctl.hasMany('list').reload())
      // .then(list => Promise.all(list.toArray()))
      // .then(transfers => tranfer.belongsTo('currentStat').reload())
      .catch(error => this.set('currentError', error))
      .finally(() => this.set('currentIsUpdating', false));
  },

});
