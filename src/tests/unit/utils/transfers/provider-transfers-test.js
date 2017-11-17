import { expect } from 'chai';
import { describe, it } from 'mocha';
import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import _ from 'lodash';
import Ember from 'ember';

const {
  get,
} = Ember;

function findEmberObject(array, properties) {
  return _.find(array, e => {
    for (let key in properties) {
      if (get(e, key) !== properties[key]) {
        return false;
      }
    }
    return true;
  });
}

const INPUT_TRANSFER_SPEED = [{
    destination: 'p0',
    bytesPerSec: {
      p1: 1,
      p2: 3,
    },
  },
  {
    destination: 'p0',
    bytesPerSec: {
      p1: 5,
    },
  },
  {
    destination: 'p1',
    bytesPerSec: {
      p0: 7,
      p2: 11,
    },
  },
  {
    destination: 'p1',
    bytesPerSec: {
      p2: 13,
    },
  },
];

describe('Unit | Utility | transfers/provider transfers', function () {
  it('generates array of input provider transfer objects from transfers collection',
    function () {
      let result = providerTransfers(INPUT_TRANSFER_SPEED);
      expect(result).to.have.lengthOf(4);
      expect(findEmberObject(result, { src: 'p1', dest: 'p0' }), 'p1->p0')
        .to.deep.include({
          src: 'p1',
          dest: 'p0',
          bytesPerSec: 6,
        });
      expect(findEmberObject(result, { src: 'p2', dest: 'p0' }), 'p2->p0')
        .to.deep.include({
          src: 'p2',
          dest: 'p0',
          bytesPerSec: 3,
        });
      expect(findEmberObject(result, { src: 'p0', dest: 'p1' }), 'p0->p1')
        .to.deep.include({
          src: 'p0',
          dest: 'p1',
          bytesPerSec: 7,
        });
      expect(findEmberObject(result, { src: 'p2', dest: 'p1' }), 'p2->p1')
        .to.deep.include({
          src: 'p2',
          dest: 'p1',
          bytesPerSec: 11 + 13,
        });
    });
});
