/**
 * FIXME: doc
 *
 * @module utils/transfer-time-stat-updater
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

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
   * @type {Looper}
   */
  _watcher: undefined,
  
  /**
   * If true, currently fetching info about current transfers
   * Set by some interval watcher
   * @type {boolean}
   */
  isUpdating: undefined,

  /**
   * Error object from fetching current transfers info
   * @type {any} typically a request error object
   */
  fetchError: null,
  
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
   * @type {number}
   */
  _interval: computed.reads('_sharedInterval'),

  // FIXME: code for this watcher is cloned from multi-watchers, so reduce it
  
  init() {
    this._super(...arguments);

    this.setProperties({
      isUpdating: true,
    });

    this._createWatchers();
    this._reconfigureWatchers();

    // get properties to enable observers
    this.getProperties('_interval');
  },

  destroy() {
    try {
      this.set('_interval', undefined);
      this.get('_watcher').destroy();
    } finally {
      this._super(...arguments);
    }
  },

  _reconfigureWatchers: observer(
    '_interval',
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
    const _watcher = Looper.create({
      immediate: true,
    });
    _watcher.on('tick', () => safeExec(this, 'fetch'));

    this.set('_watcher', _watcher);
  },

  _setWatchersIntervals() {
    // this method is invoked from debounce, so it's "this" can be destroyed
    if (this.isDestroyed === false) {
      const {
        _interval,
        _watcher,
      } = this.getProperties(
        '_interval',
        '_watcher'
      );
      set(_watcher, 'interval', _interval);
    }
  },

  // TODO: if refactoring - this can be custom, injected function
  fetch() {
    const timeStat = this.get('timeStat');
    this.set('isUpdating', true);
    
    return timeStat.reload()
      .catch(error => this.set('fetchError', error))
      .finally(() => this.set('isUpdating', false));
  },

});
