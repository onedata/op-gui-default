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
        blocksPercentage: 0,
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.true;
    });
    
    it('computes if file distribution is not empty nor complete', function() {
      const model = this.subject({
        blocksPercentage: 30,
        fileSize: FILE_SIZE,
      });
      expect(model.get('isEmpty')).to.be.false;
      expect(model.get('isComplete')).to.be.false;
    });
    
    it('computes if file distribution is complete', function () {
      const model = this.subject({
        blocksPercentage: 100,
        fileSize: FILE_SIZE,
      });
      expect(model.get('isComplete')).to.be.true;
    });
  }
);
