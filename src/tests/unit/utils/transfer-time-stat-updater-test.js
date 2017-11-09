import { expect } from 'chai';
import { describe, it } from 'mocha';
import transferTimeStatUpdater from 'op-worker-gui/utils/transfer-time-stat-updater';

describe('Unit | Utility | transfer time stats updater', function() {
  // Replace this with your real tests.
  it('works', function() {
    let result = transferTimeStatUpdater();
    expect(result).to.be.ok;
  });
});

