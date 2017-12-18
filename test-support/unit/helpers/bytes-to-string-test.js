import { expect } from 'chai';
import { describe, it } from 'mocha';
import { bytesToString } from 'ember-cli-onedata-common/helpers/bytes-to-string';

describe('Unit | Helper | bytes to string', function () {
  it('supports iecFormat option of bytes-to-string util', function () {
    let result = bytesToString([1024], { iecFormat: true });
    expect(result).to.be.equal('1 KiB');
  });

  it('uses iecFormat=true option of bytes-to-string util by default', function () {
    let result = bytesToString([1024]);
    expect(result).to.be.equal('1 KiB');
  });

  it('generates valid KB string with iecFormat=false as in bytes-to-string util',
    function () {
      let result = bytesToString([1500], { iecFormat: false });
      expect(result).to.be.equal('1.5 KB');
    }
  );
});
