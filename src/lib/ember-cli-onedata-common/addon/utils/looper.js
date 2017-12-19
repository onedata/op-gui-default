/**
 * NOTE: this is a backport of looper util from onedata-gui-common addon
 * 
 * An Ember class to maintain a single interval with an object
 *
 * Usage:
 * - set `interval` property (on create or whenever in runtime)
 * - bind `tick` event to any function to invoke on interval (`on('tick', function)`)
 * - remember to call `stop` method or `destroy` when want to stop interval
 * - `interval` can be always changed - it will start new interval timer and clear old
 *
 * @module utils/looper
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  observer,
  on,
  Evented,
  run,
} = Ember;

export default Ember.Object.extend(Evented, {
  _intervalId: null,

  interval: null,

  /**
   * If true, the tick notify will be launched right after changing interval 
   * @type {boolean}
   */
  immediate: false,

  resetInterval: on('init', observer('interval', function () {
    let {
      immediate,
      interval,
      _intervalId,
    } = this.getProperties('immediate', 'interval', '_intervalId');
    if (_intervalId != null) {
      clearInterval(_intervalId);
    }
    if (interval && interval > 0) {
      this.set(
        '_intervalId',
        setInterval(this.notify.bind(this), interval)
      );
      if (immediate) {
        this.notify();
      }
    }
  })),

  destroy() {
    try {
      this.stop();
    } catch (error) {
      console.warn('util:looper: stopping on destroy failed');
    }
    return this._super(...arguments);
  },

  stop() {
    this.set('interval', null);
  },

  notify() {
    run.later(() => this.trigger('tick'));
  }
});
