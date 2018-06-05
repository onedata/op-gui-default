import { expect } from 'chai';
import { describe, it } from 'mocha';
import fakeListRecord from 'op-worker-gui/utils/fake-list-record';

describe('Unit | Utility | fake list record', function() {
  // Replace this with your real tests.
  it('works', function() {
    let result = fakeListRecord();
    expect(result).to.be.ok;
  });
});
