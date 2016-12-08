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
  it('leaves short strings in original form if max length is the same as string length', function() {
    let origString = 'hello';
    let result = trimString([origString, origString.length]);
    expect(result).to.be.equal(origString);
  });
});
