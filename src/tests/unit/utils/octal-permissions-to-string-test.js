/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import octalPermissionsToString from 'op-worker-gui/utils/octal-permissions-to-string';

let perms = {
  644: 'rw-r--r--',
  755: 'rwxr-xr-x',
  112: '--x--x-w-',
  334: '-wx-wxr--',
  567: 'r-xrw-rwx',
  777: 'rwxrwxrwx',
};

let mochaExpectEqualAfterConvert = (octal) => {
  return () => {
    expect(octalPermissionsToString(octal)).to.be.equal(perms[octal]);
  };
};

describe('octalPermissionsToString', function() {
  for (let octal in perms) {
    it(`converts ${octal} to ${perms[octal]}`, mochaExpectEqualAfterConvert(octal));
  }
});
