/**
 * Array that fetches additional chunks of data if requesting indexes
 * that are not currently loaded
 *
 * @module utils/replacing-chunks-array
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ArraySlice from 'ember-cli-onedata-common/utils/array-slice';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import _ from 'lodash';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  observer,
  A,
  computed,
  get,
} = Ember;

export default ArraySlice.extend({
  /**
   * @virtual 
   * @type {function} `(fromIndex, size, offset) => Array<any>`
   */
  fetch: undefined,
  
  startIndex: 0,
  endIndex: 0,
  indexMargin: 0,
  
  /**
   * Initialized in init
   * @type {PromiseObject<ReplacingChunksArray>}
   */
  initialLoad: undefined,
  
  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  isLoaded: computed.reads('initialLoad.isSettled'),
  
  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  isLoading: computed.not('isLoaded'),
  
  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  isReloading: computed.reads('_isReloading'),
  
  /**
   * @type {Ember.ComputedProperty<number>}
   */
  chunkSize: computed.reads('maxLength'),
  
  loadMoreThreshold: computed('chunkSize', function getLoadMoreThreshold() {
    return this.get('chunkSize') / 2;
  }),
  
  // TODO: implement better start change handling
  /**
   * @type {boolean}
   */
  _startReached: true,
  
  // TODO: implement better end change handling
  /**
   * @type {boolean}
   */
  _endReached: false,
  
  /**
   * Prevents infinite recursion when fetching new data
   * @type {boolean}
   */
  _fetchNextLock: false,
  
  /**
   * Prevents infinite recursion when fetching new data
   * @type {boolean}
   */
  _fetchPrevLock: false,
    
  /**
   * Set to true if reloading is in progress
   * @type {boolean}
   */
  _isReloading: false,
  
  startEndChanged: observer(
    '_start',
    '_end',
    '_startReached',
    '_endReached',
    'loadMoreThreshold',
    'sourceArray.length',
    function observeStartEndChanged() {
      if (!this.get('isReloading')) {
        const {
          _start,
          _end,
          loadMoreThreshold,
          _startReached,
          _endReached,
          sourceArray,
        } = this.getProperties(
          '_start',
          '_end',
          'loadMoreThreshold',
          '_startReached',
          '_endReached',
          'sourceArray'
        );
        if (!_startReached && _start < loadMoreThreshold) {
          this.fetchPrev();
        }
        if (!_endReached && _end + loadMoreThreshold >= get(sourceArray, 'length')) {
          this.fetchNext();
        }
      }
    }),

  
  fetchPrev() {
    if (!this.get('_fetchPrevLock')) {
      this.set('_fetchPrevLock', true);
      /** @type {Ember.ArrayProxy} */
      const sourceArray = this.get('sourceArray');
      /** @type {number} */
      const chunkSize = this.get('chunkSize');
      
      return this.get('fetch')(
        get(sourceArray, 'firstObject.index'),
        chunkSize,
        -chunkSize
      ).then(array => {
        if (get(array, 'length') < chunkSize) {
          safeExec(this, 'set', '_startReached', true);
        }
        array = _.difference(array, sourceArray);
        sourceArray.unshift(...array);
        sourceArray.sortBy('listIndex');
        sourceArray.arrayContentDidChange();
      }).finally(() => safeExec(this, 'set', '_fetchPrevLock', false));
    }
  },
  
  fetchNext() {
    if (!this.get('_fetchNextLock')) {
      this.set('_fetchNextLock', true);
      /** @type {Ember.ArrayProxy} */
      const sourceArray = this.get('sourceArray');
      /** @type {number} */
      const chunkSize = this.get('chunkSize');
      return this.get('fetch')(
        get(sourceArray, 'lastObject.index'),
        chunkSize,
        0
      ).then(array => {
        if (get(array, 'length') < chunkSize) {
          safeExec(this, 'set', '_endReached', true);
        }
        array = _.difference(array, sourceArray);
        sourceArray.push(...array);
        sourceArray.sortBy('listIndex');
        sourceArray.arrayContentDidChange();
      }).finally(() => safeExec(this, 'set', '_fetchNextLock', false));
    }
  },
  
  reload({ head = false, minSize = 0 } = {}) {
    const {
      _start,
      _end,
      startIndex,
      endIndex,
      sourceArray,
    } = this.getProperties(
      '_start',
      '_end',
      'startIndex',
      'endIndex',
      'sourceArray'
    );

    let reloadStart = Math.min(_start, startIndex);
    let reloadEnd = Math.max(_end, endIndex);
    if (reloadEnd === -1) {
      reloadEnd = 0;
    }
    if (reloadStart === -1) {
      reloadStart = 0;
    }
    let size = reloadEnd - reloadStart;
    if (size < minSize) {
      size = minSize;
    }
    
    this.set('_isReloading', true);
    return this.get('fetch')(
      head ? null : this.get('firstObject.index'),
      size,
      0
    ).then(updatedRecordsArray => {
      safeExec(this, 'setProperties', {
        _startReached: true,
        _endReached: false,
      });
      sourceArray.clear();
      sourceArray.pushObjects(updatedRecordsArray);
      return this;
    }).finally(() => safeExec(this, 'set', '_isReloading', false));
  },
  
  init() {
    this.set('sourceArray', A());
    this._super(...arguments);
    this.set(
      'initialLoad',
      PromiseObject.create({ promise: this.reload() })
    );
  },
});