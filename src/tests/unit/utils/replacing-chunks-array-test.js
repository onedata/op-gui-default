import { expect } from 'chai';
import { describe, it } from 'mocha';
import ReplacingChunksArray from 'op-worker-gui/utils/replacing-chunks-array';
import _ from 'lodash';
import sinon from 'sinon';
import Ember from 'ember';
import wait from 'ember-test-helpers/wait';

const {
  get,
  RSVP: { Promise },
} = Ember;

class Record {
  constructor(index) {
    this.index = index;
  }
}

function recordRange(start, end) {
  return _.range(start, end).map(i => new Record(i));
}

const testArray = recordRange(0, 1000);

function fetch(fromId = 0, size = Number.MAX_SAFE_INTEGER, offset = 0) {
  let startIndex = 0;
  for (let i = 0; i < testArray.length; ++i) {
    startIndex = i;
    if (testArray[i].index >= fromId) {
      break;
    }
  }
  const startOffset = Math.max(0, Math.min(startIndex + offset, testArray.length - size));
  return Promise.resolve(testArray.slice(startOffset, startOffset + size));
}

describe('Unit | Utility | replacing chunks array', function() {
  it('exposes fragment of internal array and fetches new chunks', function() {
    const fetchSpy = sinon.spy(fetch);
    const array = ReplacingChunksArray.create({
      fetch: fetchSpy,
      startIndex: 0,
      endIndex: 10,
    });
    get(array, 'initialLoad').then(() => {
      expect(fetchSpy, 'initial fetch').to.be.calledOnce;
      expect(get(array, 'length'), 'length after init').to.equal(10);
      expect(array.toArray(), 'content after init').to.deep.equal(recordRange(0, 10));
      
      array.setProperties({
        startIndex: 7,
        endIndex: 17,
      });
      return wait().then(() => {
        expect(fetchSpy, 'fetch after index change').to.be.calledTwice;
        expect(get(array, 'length'), 'length after index change').to.equal(10);
        expect(array.toArray(), 'content after index change')
          .to.deep.equal(recordRange(7, 17));
      });
    });
  });
  
  it('reloads currently viewed fragment of array', function() {
    const fetchSpy = sinon.spy(fetch);
    const array = ReplacingChunksArray.create({
      fetch: fetchSpy,
      startIndex: 0,
      endIndex: 10,
    });
    return wait().then(() => {
      expect(fetchSpy, 'initial fetch').to.be.calledOnce;
      array.reload();
      return wait().then(() => {
        expect(fetchSpy, 'fetch after reload').to.be.calledTwice;
        expect(fetchSpy, 'fetch current records').to.be.calledWith(0, 10, 0);
        expect(array.toArray(), 'content after reload')
        .to.deep.equal(recordRange(0, 10));
      });
    });
  });
});