import { expect } from 'chai';
import { describe, it } from 'mocha';
import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import _ from 'lodash';

const ONE_MB = Math.pow(1024, 2);

const INPUT_TRANSFER_SPEED = [{
    dest: 'p0',
    bytesPerSec: {
      p1: 1 * ONE_MB,
      p2: 3 * ONE_MB,
    },
  },
  {
    dest: 'p0',
    bytesPerSec: {
      p1: 5 * ONE_MB,
    },
  },
  {
    dest: 'p1',
    bytesPerSec: {
      p0: 7 * ONE_MB,
      p2: 11 * ONE_MB,
    },
  },
  {
    dest: 'p1',
    bytesPerSec: {
      p2: 13 * ONE_MB,
    },
  },
];

describe('Unit | Utility | transfers/provider transfers', function () {
  it('generates array of input provider transfer objects from transfers collection',
    function () {
      let result = providerTransfers(INPUT_TRANSFER_SPEED);
      expect(result).to.have.lengthOf(4);
      expect(_.find(result, { src: 'p1', dest: 'p0' }), 'p1->p0')
        .to.deep.equal({
          src: 'p1',
          dest: 'p0',
          bytesPerSec: 6 * ONE_MB,
        });
      expect(_.find(result, { src: 'p2', dest: 'p0' }), 'p2->p0')
        .to.deep.equal({
          src: 'p2',
          dest: 'p0',
          bytesPerSec: 3 * ONE_MB,
        });
      expect(_.find(result, { src: 'p0', dest: 'p1' }), 'p0->p1')
        .to.deep.equal({
          src: 'p0',
          dest: 'p1',
          bytesPerSec: 7 * ONE_MB,
        });
      expect(_.find(result, { src: 'p2', dest: 'p1' }), 'p2->p1')
        .to.deep.equal({
          src: 'p2',
          dest: 'p1',
          bytesPerSec: (11 + 13) * ONE_MB,
        });
    });
});
