import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';

describe('Unit | Component | file chunks bar', function() {
  setupComponentTest('file-chunks-bar', {
    needs: ['util:file-chunks-bar', 'helper:eq'],
    unit: true
  });

  beforeEach(function () {
    this.file = {
      id: 'file-id-1',
      sizeHumanReadable: '1 KiB',
      size: 1024,
    };
  });
  
  it('computes 25.2% chunksPercent for 25.15123 blocksPercentage', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('blocksPercentage', 25.15123);
    expect(component.get('chunksPercent')).to.equal('25.2%');
  });
  
  it('computes 25.1% chunksPercent for 25.1498 blocksPercentage', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('blocksPercentage', 25.1498);
    expect(component.get('chunksPercent')).to.equal('25.1%');
  });
});
