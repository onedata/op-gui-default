/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import {
  safeElementId
} from 'ember-cli-onedata-common/helpers/safe-element-id';

describe('SafeElementIdHelper', function() {
  it('accepts an original id as a first array argument and returns converted id without forbidden chars', function() {
    let result = safeElementId(['#$@$#*RYEdsffdsuhg37q3a-d#-a3-efajf983q$']);
    expect(result).to.be.equal('RYEdsffdsuhg37q3a-d-a3-efajf983q');
  });
});
