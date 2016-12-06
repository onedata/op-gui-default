/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import safeElementId from 'op-worker-gui/utils/safe-element-id';

describe('safeElementId', function() {
  // Replace this with your real tests.
  it('works', function() {
    let result = safeElementId();
    expect(result).to.be.ok;
  });
});
