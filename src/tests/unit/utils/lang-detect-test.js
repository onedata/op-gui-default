/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import langDetect from 'op-worker-gui/utils/lang-detect';

describe('langDetect', function() {
  it('returns always "en" in version 3.0', function() {
    let result = langDetect();
    expect(result).to.be.equal('en');
  });
});
