/**
 * An array proxy that exposes only selected slice of real EmberArray.
 * 
 * See tests for usage examples.
 *
 * @module utils/array-slice
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  ArrayProxy,
  computed,
  observer,
} = Ember;

export default ArrayProxy.extend({
  startIndex: 0,
  endIndex: 0,
  indexMargin: 0,
  
  _startCache: undefined,
  _endCache: undefined,
  
  sourceArray: computed.alias('content'),
  
  /**
   * @type {Ember.ComputedProperty<number>}
   */
  maxLength: computed('startIndex', 'endIndex', function getMaxLength() {
    return this.get('endIndex') - this.get('startIndex');
  }),
  
  _start: computed('startIndex', 'indexMargin', function () {
    const {
      startIndex,
      indexMargin,
    } = this.getProperties(
      'startIndex',
      'indexMargin'
    );
    const _start = Math.max(0, startIndex - indexMargin);
    
    this.set('_startCache', _start);
    return _start;
  }),
  
  _end: computed('endIndex', 'indexMargin', 'sourceArray.length', function () {
    const {
      endIndex,
      indexMargin,
    } = this.getProperties(
      'endIndex',
      'indexMargin'
    );
    
    const sourceLength = this.get('sourceArray.length');
    return Math.min(sourceLength, endIndex + indexMargin);
  }),
  
  _startChanged: observer('_start', function _startChanged() {
    const {
      _startCache,
      _start,
    } = this.getProperties(
      '_startCache',
      '_start'
    );
    
    if (_startCache !== undefined && _start !== _startCache) {
      let removeAmt = 0;
      let addAmt = 0;
      if (_start > _startCache) {
        removeAmt = _start - _startCache;
      } else {
        addAmt = _startCache - _start;
      }
      this.arrayContentDidChange(_start, removeAmt, addAmt);
    }
    
    this.set('_startCache', _start);
  }),
  
  _endChanged: observer('_end', function _endChanged() {
    const {
      _endCache,
      _end,
    } = this.getProperties(
      '_endCache',
      '_end'
    );
    if (_endCache !== undefined && _end !== _endCache) {
      let removeAmt = 0;
      let addAmt = 0;
      if (_end > _endCache) {
        addAmt = _end - _endCache;
      } else {
        removeAmt = _endCache - _end;
      }
      this.arrayContentDidChange(_endCache - removeAmt, removeAmt, addAmt);
    }
    
    this.set('_endCache', _end);
  }),
  
  /**
   * @override 
   */
  replace(idx, amt, objects) {
    const sourceArray = this.get('sourceArray');
    return sourceArray.replace(this._translateIndex(idx), amt, objects);
  },
  
  /**
   * @override
   */
  objectAt(idx) {
    const sourceArray = this.get('sourceArray');
    if (sourceArray) {
      return sourceArray.objectAt(this._translateIndex(idx));
    }
  },
  
  /**
   * @override 
   */
  length: computed('_start', '_end', function () {
    const {
      _start,
      _end,
    } = this.getProperties(
      '_start',
      '_end'
    );
    return _end - _start;
  }),

  _arrayContentChange(startIdx, removeAmt, addAmt, fun) {
    const {
      _start,
      _end,
    } = this.getProperties(
      '_start',
      '_end'
    );
    if (_start <= startIdx && startIdx <= _end) {
      const sliceStartIdx = startIdx - _start;
      const sliceRemoveAmt = Math.min(_end, sliceStartIdx + removeAmt) - sliceStartIdx;
      const sliceAddAmt = Math.min(_end, sliceStartIdx + addAmt) - sliceStartIdx;
      return fun.bind(this)(sliceStartIdx, sliceRemoveAmt, sliceAddAmt);
    } else {
      return this;
    }
  },
  
  /**
   * @override
   */
  arrayContentWillChange(startIdx, removeAmt, addAmt) {
    return this._arrayContentChange(startIdx, removeAmt, addAmt, this._super);
  },
  
  /**
   * @override
   */
  arrayContentDidChange(startIdx, removeAmt, addAmt) {
    return this._arrayContentChange(startIdx, removeAmt, addAmt, this._super);
  },
  
  _translateIndex(index) {
    const {
      _start,
      _end,
    } = this.getProperties(
      '_start',
      '_end'
    );
    const translatedIndex = _start + index;
    return translatedIndex > _end ? -1 : translatedIndex;
  },
  
  init() {
    this._super(...arguments);
    // activate observers
    this.getProperties('_start', '_end');
    this._startChanged();
    this._endChanged();
  },
});
