import { expect } from 'chai';
import { describe, it } from 'mocha';
import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import _ from 'lodash';

const ONE_MB = Math.pow(1024, 2);

const TRANSFERS = [{
    fileName: 'f1',
    destination: 'p0',
    stats: {
      hour: {
        p1: [1 * ONE_MB],
        p2: [3 * ONE_MB],
      },
    }
  },
  {
    fileName: 'f2',
    destination: 'p0',
    stats: {
      hour: {
        p1: [5 * ONE_MB],
      },
    }
  },
  {
    fileName: 'f3',
    destination: 'p1',
    stats: {
      hour: {
        p0: [7 * ONE_MB],
        p2: [11 * ONE_MB],
      },
    }
  },
  {
    fileName: 'f4',
    destination: 'p1',
    stats: {
      hour: {
        p2: [13 * ONE_MB],
      }
    }
  },
];

describe('Unit | Utility | transfers/provider transfers', function () {
  it('generates array of input provider transfer objects from transfers collection',
    function () {
      let result = providerTransfers(TRANSFERS);
      expect(result).to.have.lengthOf(4);
      expect(_.find(result, { src: 'p1', dest: 'p0' })).to.deep.equal({
        src: 'p1',
        dest: 'p0',
        bytesPerSec: (6 * ONE_MB) / 60,
      });
      expect(_.find(result, { src: 'p2', dest: 'p0' })).to.deep.equal({
        src: 'p2',
        dest: 'p0',
        bytesPerSec: (3 * ONE_MB) / 60,
      });
      expect(_.find(result, { src: 'p0', dest: 'p1' })).to.deep.equal({
        src: 'p0',
        dest: 'p1',
        bytesPerSec: (7 * ONE_MB) / 60,
      });
      expect(_.find(result, { src: 'p2', dest: 'p1' })).to.deep.equal({
        src: 'p2',
        dest: 'p1',
        bytesPerSec: ((11 + 13) * ONE_MB) / 60,
      });
    });
});
