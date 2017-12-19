import { expect } from 'chai';
import { describe, it } from 'mocha';
import reduceArrays from 'op-worker-gui/utils/reduce-arrays';

describe('Unit | Utility | reduce arrays', function() {
  it('can be used to sum multiple arrays elements', function() {
    let result = reduceArrays(
      [1, 2, 3],
      [10, 20, 30],
      [100, 200, 300]
    );
    expect(result).to.deep.equal([111, 222, 333]);
  });
});
