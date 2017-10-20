import { expect } from 'chai';
import { describe, it } from 'mocha';
import spaceTransfers from 'op-worker-gui/utils/transfers/space-transfers';
import _ from 'lodash';

const ONE_MB = Math.pow(1024, 2);

const P1_P0 = (1 * ONE_MB) / 60;
const P0_P1 = (5 * ONE_MB) / 60;
const P2_P0 = (3 * ONE_MB) / 60;
const P2_P1 = (7 * ONE_MB) / 60;

const PROVIDER_TRANSFERS = [{
    src: 'p1',
    dest: 'p0',
    bytesPerSec: P1_P0,
  },
  {
    src: 'p2',
    dest: 'p0',
    bytesPerSec: P2_P0,
  },
  {
    src: 'p0',
    dest: 'p1',
    bytesPerSec: P0_P1,
  },
  {
    src: 'p2',
    dest: 'p1',
    bytesPerSec: P2_P1,
  },
];

describe('Unit | Utility | transfers/space transfers', function () {
  it('converts array of providerTransfers into array of SpaceOutputTransfers', function () {
    const result = spaceTransfers(PROVIDER_TRANSFERS, 'out');
    expect(result).to.have.lengthOf(3);
    expect(_.find(result, { providerId: 'p0' }).bytesPerSec).to.equal(P0_P1);
    expect(_.find(result, { providerId: 'p1' }).bytesPerSec).to.equal(P1_P0);
    expect(_.find(result, { providerId: 'p2' }).bytesPerSec).to.equal(P2_P0 + P2_P1);
  });
  
  it('converts array of providerTransfers into array of SpaceInputTransfers', function () {
    const result = spaceTransfers(PROVIDER_TRANSFERS, 'in');
    expect(result).to.have.lengthOf(2);
    expect(_.find(result, { providerId: 'p0' }).bytesPerSec).to.equal(P1_P0 + P2_P0);
    expect(_.find(result, { providerId: 'p1' }).bytesPerSec).to.equal(P0_P1 + P2_P1);
  });
});
