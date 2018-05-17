import { expect } from 'chai';
import { describe, it } from 'mocha';
import hasDuplicatedFileChunks from 'op-worker-gui/utils/has-duplicated-file-chunks';

const fileSize = 95;

const compareChunks = [
  [0, 10],
  [0, 20, 25, 40, 60, 70],
  [15, 23, 25, 26, 27, 41, 43, 55, 56, 90],
];

const wholeFileCompareChunks = [
  [0, 10],
  [0, 20, 25, 40, 60, 70],
  [15, 26, 27, 60, 56, 95],
];

describe('Unit | Utility | has duplicated file chunks', function() {
  it('[partial] returns true for duplicated file chunks', function() {
    const subjectChunks = [50, 57, 92, 94];
    const result = hasDuplicatedFileChunks(fileSize, subjectChunks, compareChunks, false);
    expect(result).to.be.true;
  });

  it('[partial] returns false for non-duplicated file chunks', function() {
    const subjectChunks = [42, 42, 92, 94];
    const result = hasDuplicatedFileChunks(fileSize, subjectChunks, compareChunks, false);
    expect(result).to.be.false;
  });

  it('[whole file] returns true for duplicated file chunks', function() {
    const subjectChunks = [50, 57, 92, 94];
    const result = hasDuplicatedFileChunks(fileSize, subjectChunks, wholeFileCompareChunks);
    expect(result).to.be.true;
  });

  it('[whole file] returns false for non-duplicated file chunks', function() {
    const subjectChunks = [42, 42, 92, 94];
    const result = hasDuplicatedFileChunks(fileSize, subjectChunks, compareChunks);
    expect(result).to.be.false;
  });
});