import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import wait from 'ember-test-helpers/wait';

const P1 = {
  id: 'p1',
  name: 'Provider 1',
};

const FILE_DISTRIBUTION_F1_NS = {
  file: {
    id: 'f1',
    name: 'File 1',
    size: 100,
  },
  provider: P1.id,
  getProvider: P1,
  isEmpty: true,
  isComplete: false,
  neverSynchronized: false,
};


describe('Integration | Component | modals/file chunks/provider row', function() {
  setupComponentTest('modals/file-chunks/provider-row', {
    integration: true
  });

  it('renders provider name', function() {
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      currentProviderSupport=true
      fileTransfers=fileTransfers
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      expect($providerRow).to.exist;
      expect($providerRow.text()).to.match(/.*Provider 1.*/);
    });
  });
  
  it('renders migration button as disabled when file was never synchronized', function() {
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      currentProviderSupport=true
      fileTransfers=fileTransfers
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnReplicate = $providerRow.find('.btn-migrate');
      expect($btnReplicate).to.exist;
      expect($btnReplicate, $btnReplicate[0].className).to.have.class('disabled');
    });
  });
});
