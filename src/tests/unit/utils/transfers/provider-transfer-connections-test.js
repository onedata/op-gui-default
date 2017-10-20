import { expect } from 'chai';
import { describe, it } from 'mocha';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';

import _ from 'lodash';

const ONE_MB = Math.pow(1024, 2);

describe('Unit | Utility | transfers/provider transfer connections', function() {
  it('generates an array of undirected connection between providers', function() {
    const providerTransfers = [
      {
        src: 'p1',
        dest: 'p0',
        bytesPerSec: (1*ONE_MB)/60,
      },
      {
        src: 'p2',
        dest: 'p0',
        bytesPerSec: (3*ONE_MB)/60,
      },
      {
        src: 'p0',
        dest: 'p1',
        bytesPerSec: (5*ONE_MB)/60,
      },
      {
        src: 'p2',
        dest: 'p1',
        bytesPerSec: (7*ONE_MB)/60,
      },
    ];
    
    let result = providerTransferConnections(providerTransfers);
    expect(result).to.have.length(3);
    expect(_.find(result, pair => _.isEqual(pair, ['p0', 'p1'])), 'p0-p1')
      .to.be.ok;
    expect(_.find(result, pair => _.isEqual(pair, ['p0', 'p2'])), 'p0-p2')
      .to.be.ok;
    expect(_.find(result, pair => _.isEqual(pair, ['p1', 'p2'])), 'p1-p2')
      .to.be.ok;
  });
});
