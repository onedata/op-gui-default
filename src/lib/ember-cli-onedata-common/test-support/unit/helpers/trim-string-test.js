/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import {
  trimString
} from 'op-worker-gui/helpers/trim-string';

describe('TrimStringHelper', function() {
  // Replace this with your real tests.
  it('works', function() {
    let result = trimString(42);
    expect(result).to.be.ok;
  });
});
