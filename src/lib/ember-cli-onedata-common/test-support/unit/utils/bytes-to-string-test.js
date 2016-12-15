/* jshint expr:true */
import { expect } from 'chai';
import { describe, it } from 'mocha';
import bytesToString from 'op-worker-gui/utils/bytes-to-string';

describe('Unit | Utility | bytes to string', function() {
  it('can convert byte size to GiB string', function() {
    let result = bytesToString(1073741824, {iecFormat: true});
    expect(result).to.be.equal('1 GiB');
  });

  it('can convert byte size to MiB string', function() {
    let result = bytesToString(1048576, {iecFormat: true});
    expect(result).to.be.equal('1 MiB');
  });

  it('can convert byte size to GB string with default options', function() {
    let result = bytesToString(1000000000);
    expect(result).to.be.equal('1 GB');
  });

  it('can convert byte size to MB string with default options', function() {
    let result = bytesToString(1000000);
    expect(result).to.be.equal('1 MB');
  });

  it('rounds size in string to 1 digit after comma by default', function() {
    let result = bytesToString(1251111);
    expect(result).to.be.equal('1.3 MB');
  });
});
