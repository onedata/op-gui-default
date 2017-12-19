import { expect } from 'chai';
import { describe, it } from 'mocha';
import fileName from 'op-worker-gui/utils/file-name';

describe('Unit | Utility | file name', function() {
  it('parses file name from POSIX path with subdirs', function() {
    let result = fileName('/one/two/three.zip');
    expect(result).to.equal('three.zip');
  });
  it('parses file name in root of filesystem', function () {
    let result = fileName('/file.zip');
    expect(result).to.equal('file.zip');
  });
});
