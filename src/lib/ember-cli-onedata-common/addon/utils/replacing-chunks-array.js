import Ember from 'ember';
import ArraySlice from 'ember-cli-onedata-common/utils/array-slice';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import _ from 'lodash';

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
   * @type {Promise<ReplacingChunksArray>}
   */
  initialLoad: undefined,
  
  isLoaded: computed.reads('initialLoad.isSettled'),
  isLoading: computed.not('isLoaded'),
  
  /**
   * @type {Ember.ComputedProperty<number>}
   */
  chunkSize: computed.reads('maxLength'),
  
  loadMoreThreshold: computed('chunkSize', function getLoadMoreThreshold() {
    return this.get('chunkSize') / 2;
  }),
  
  // FIXME: remember to set this to false if needed
  _startReached: true,
  // FIXME: remember to set this to false if needed
  _endReached: false,
  
  startEndChanged: observer(
    '_start',
    '_end',
    '_startReached',
    '_endReached',
    'loadMoreThreshold',
    'sourceArray.length',
    function observeStartEndChanged() {
      if (!this.get('_arrayLocked')) {
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
      // FIXME: safe method exec
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
          this.set('_startReached', true);
        }
        array = _.difference(array, sourceArray);
        sourceArray.unshiftObjects(array);
      }).finally(() => this.set('_fetchPrevLock', false));
    }
  },
  
  fetchNext() {
    if (!this.get('_fetchNextLock')) {
      this.set('_fetchNextLock', true);
      // FIXME: safe method exec
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
          this.set('_endReached', true);
        }
        array = _.difference(array, sourceArray);
        sourceArray.pushObjects(array);
      }).finally(() => this.set('_fetchNextLock', false));
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
    
    this.set('_arrayLocked', true);
    return this.get('fetch')(
      head ? null : this.get('firstObject.index'),
      size,
      0
    ).then(updatedRecordsArray => {
      sourceArray.clear();
      sourceArray.pushObjects(updatedRecordsArray);
      return this;
    }).finally(() => safeExec(this, 'set', '_arrayLocked', false));
  },
  
  init() {
    this.set('sourceArray', A());
    this._super(...arguments);
    this.set('initialLoad', this.reload());
  },
});