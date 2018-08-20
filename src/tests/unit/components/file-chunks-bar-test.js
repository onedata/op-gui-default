import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';

const barWidth = 320;
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
  
  it('computes 100% chunksPercent for full file', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      '0': 100,
    });
    expect(component.get('chunksPercent')).to.equal('100%');
  });
  
  it('computes 50% chunksPercent for half of file', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      '0': 100,
      [String(320/2)]: 0,
    });
    expect(component.get('chunksPercent')).to.equal('50%');
  });
  
  it('computes 25% chunksPercent if half of file has 50%', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      '0': 50,
      [String(320/2)]: 0,
    });
    expect(component.get('chunksPercent')).to.equal('25%');
  });
  
  it('computes 75% chunksPercent if half of file has 100% and another half has 50%', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      '0': 100,
      [String(320/2)]: 50,
    });
    expect(component.get('chunksPercent')).to.equal('75%');
  });
  
  it('computes chunksPercent for fragment in the middle', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      '0': 0,
      [String(320/4)]: 50,
      [String(320*(3/4))]: 0,
    });
    expect(component.get('chunksPercent')).to.equal('25%');
  });
  
  it('computes chunksPercent for fragment in the middle without ordered keys', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      [String(barWidth*(3/4))]: 0,
      [String(320/4)]: 50,
      '0': 0,
    });
    expect(component.get('chunksPercent')).to.equal('25%');
  });
  
  it('computes < 0.1% chunksPercent for very small chunk', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      '0': 1,
      '1': 0,
    });
    expect(component.get('chunksPercent')).to.equal('< 0.1%');
  });
  
  it('computes 0.1% chunksPercent for small chunk', function() {
    const component = this.subject();
    component.set('file', this.file);
    component.set('chunksBarData', {
      '0': 40,
      '1': 0,
    });
    expect(component.get('chunksPercent')).to.equal('0.1%');
  });
});
