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
   * Will be one of: minute, hour, day
   * @type {Ember.ComputedProperty<string>}
   */
  timespan: computed.reads('timeStat.type'),
  
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
   * Interval of time stats polling
   * @type {number|null}
   */
  _interval: computed('isEnabled', 'timespan', function () {
    if (this.get('isEnabled')) {
      switch (this.get('timespan')) {
        case 'minute':
          return 5*1000;
        case 'hour':
          return 60*1000;
        case 'day':
          return 60*60*1000;
        default:
          return null;
      }
    } else {
      return null;
    }
  }),

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
      const _watcher = this.get('_watcher');
      if (_watcher) {
        this.get('_watcher').destroy();
      }
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

  fetch() {
    const timeStat = this.get('timeStat');
    this.set('isUpdating', true);
    
    return timeStat.reload()
      .catch(error => !this.get('isDestroyed') && this.set('fetchError', error))
      .finally(() => !this.get('isDestroyed') && this.set('isUpdating', false));
  },

});
