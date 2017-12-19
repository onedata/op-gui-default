import { expect } from 'chai';
import { describeModel, it } from 'ember-mocha';

const FILE_SIZE = 100;

describeModel(
  'file-distribution',
  'Unit | Model | file distribution',
  {
    needs: ['model:file'],
  },
  function() {
    it('computes if there are no file blocks', function () {
      const model = this.subject({
        blocks: [],
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.true;
    });
    
    it('computes if file distribution is not empty nor complete', function() {
      const model = this.subject({
        blocks: [10, 20],
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.false;
      expect(model.get('isComplete')).to.be.false;
    });
    
    it('computes if file distribution is complete', function () {
      const model = this.subject({
        blocks: [0, 10, 10, 90, 90, FILE_SIZE],
        fileSize: FILE_SIZE,
      });
      expect(model.get('isComplete')).to.be.true;
    });
    
    it('detects that file is empty even if it has entries in block array', function () {
      const model = this.subject({
        blocks: [0, 0, 10, 10],
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.true;
    });
    
    it('detects that file is not complete if it has "empty" entries', function () {
      const model = this.subject({
        blocks: [0, 0],
        fileSize: FILE_SIZE,
      });
      expect(model.get('isComplete')).to.be.false;
    });
  }
);
