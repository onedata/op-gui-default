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
  isEmpty: false,
  isComplete: false,
  neverSynchronized: false,
};


describe('Integration | Component | modals/file chunks/provider row', function () {
  setupComponentTest('modals/file-chunks/provider-row', {
    integration: true
  });
  
  it('renders provider name', function (done) {
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      fileTransfers=fileTransfers
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      expect($providerRow).to.exist;
      expect($providerRow.text()).to.match(/.*Provider 1.*/);
      done();
    });
  });
    
  it('renders transfer icons as disabled if transfersEnabled is false', function (done) {
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      fileTransfers=fileTransfers
      transferEnabled=false
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnMigrate = $providerRow.find('.btn-migrate');
      const $btnReplicate = $providerRow.find('.btn-replicate');
            
      expect($btnMigrate).to.be.visible;
      expect($btnReplicate).to.be.visible;
      expect($btnMigrate, 'btn-migrate').to.have.class('disabled');
      expect($btnReplicate, 'btn-replicate').to.have.class('disabled');
      
      done();
    });
  });
  
  it('renders transfer icons as enabled if transferEnabled if true', function (done) {
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      fileTransfers=fileTransfers
      transferEnabled=true
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnMigrate = $providerRow.find('.btn-migrate');
      const $btnReplicate = $providerRow.find('.btn-replicate');
      
      expect($btnMigrate).to.be.visible;
      expect($btnReplicate).to.be.visible;
      expect($btnMigrate, 'btn-migrate').to.not.have.class('disabled');
      expect($btnReplicate, 'btn-replicate').to.not.have.class('disabled');
      
      // TODO: check clickability
      
      done();
    });
  });
});
