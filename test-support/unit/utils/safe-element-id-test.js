/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import safeElementId from 'op-worker-gui/utils/safe-element-id';

describe('safeElementId', function() {
  it('returns id with only alphanumerical chars and dashes', function() {
    let result = safeElementId('#$@$#*RYEdsffdsuhg37q3a-d#-a3-efajf983q$');
    expect(result).to.be.equal('RYEdsffdsuhg37q3a-d-a3-efajf983q');
  });
});
