/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import {
  safeElementId
} from 'op-worker-gui/helpers/safe-element-id';

describe('SafeElementIdHelper', function() {
  // Replace this with your real tests.
  it('works', function() {
    let result = safeElementId(42);
    expect(result).to.be.ok;
  });
});
