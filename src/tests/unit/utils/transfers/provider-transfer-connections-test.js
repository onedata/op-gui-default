import { expect } from 'chai';
import { describe, it } from 'mocha';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';

import _ from 'lodash';

describe('Unit | Utility | transfers/provider transfer connections', function() {
  it('generates an array of undirected connection between providers', function() {
    const mapping = {
      'p1': ['p0'],
      'p2': ['p0', 'p1'],
      'p0': ['p1'],
    };
    
    let result = providerTransferConnections(mapping);
    expect(result).to.have.length(3);
    expect(_.find(result, pair => _.isEqual(pair, ['p0', 'p1'])), 'p0-p1')
      .to.be.ok;
    expect(_.find(result, pair => _.isEqual(pair, ['p0', 'p2'])), 'p0-p2')
      .to.be.ok;
    expect(_.find(result, pair => _.isEqual(pair, ['p1', 'p2'])), 'p1-p2')
      .to.be.ok;
  });
});
