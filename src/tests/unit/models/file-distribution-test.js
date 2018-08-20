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
        chunksBarData: {},
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.true;
    });
    
    it('computes if file distribution is not empty nor complete', function() {
      const model = this.subject({
        chunksBarData: {
          10: 100,
          20: 0,
        },
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.false;
      expect(model.get('isComplete')).to.be.false;
    });
    
    it('computes if file distribution is complete', function () {
      const model = this.subject({
        chunksBarData: { 0: 100, 50: 100 },
        fileSize: FILE_SIZE,
      });
      expect(model.get('isComplete')).to.be.true;
    });
    
    it('detects that file is empty even if it has entries in chunksBarData', function () {
      const model = this.subject({
        chunksBarData: { 0: 0, 50: 0 },
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.true;
    });
    
    it('detects that file is not complete if it has "empty" entries', function () {
      const model = this.subject({
        chunksBarData: { 0: 0 },
        fileSize: FILE_SIZE,
      });
      expect(model.get('isComplete')).to.be.false;
    });
  }
);
