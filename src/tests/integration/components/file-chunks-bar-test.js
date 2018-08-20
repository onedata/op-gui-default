import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | file chunks bar', function() {
  setupComponentTest('file-chunks-bar', {
    integration: true
  });

  beforeEach(function () {
    this.set('file', {
      id: 'file-id-1',
      sizeHumanReadable: '1 KiB',
      size: 1024,
    });
  });
  
  it('renders with file size', function () {
    this.set('chunksBarData', {});
    this.render(hbs`{{file-chunks-bar
      file=file
      chunksBarData=chunksBarData
    }}`);
    
    const $fileChunksBar = this.$('.file-chunks-bar');
    
    expect($fileChunksBar).to.exist;
    expect($fileChunksBar).to.contain('1 KiB');
  });
  
  it('computes data 0% for empty chunksBarData', function () {
    this.set('chunksBarData', {});
    this.render(hbs`{{file-chunks-bar
      file=file
      chunksBarData=chunksBarData
    }}`);
    
    const $fileChunksBar = this.$('.file-chunks-bar');
    
    expect($fileChunksBar).to.exist;
    expect($fileChunksBar).to.contain('0%');
  });
});
