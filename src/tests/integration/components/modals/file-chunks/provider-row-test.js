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


describe('Integration | Component | modals/file chunks/provider row', function () {
  setupComponentTest('modals/file-chunks/provider-row', {
    integration: true
  });

  it('renders provider name', function(done) {
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
      done();
    });
  });
  
  it('renders migration button as disabled when file was never synchronized', function (done) {
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
      const $btnMigrate = $providerRow.find('.btn-migrate');
      expect($btnMigrate).to.exist;
      expect($btnMigrate, $btnMigrate[0].className).to.have.class('disabled');
      done();
    });
  });
  
  it('renders transfer icons if renderTransfers if true', function (done) {
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      currentProviderSupport=true
      fileTransfers=fileTransfers
      renderTransferIcons=true
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      expect($providerRow.find('.btn-migrate')).to.be.visible;
      expect($providerRow.find('.btn-replicate')).to.be.visible;
      done();
    });
  });
  
  it('does not render transfer icons if renderTransfers if false', function (done) {
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      currentProviderSupport=true
      fileTransfers=fileTransfers
      renderTransferIcons=false
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      expect($providerRow.find('.btn-migrate'), 'no migrate button')
        .to.not.exist;
      expect($providerRow.find('.btn-replicate'), 'no replicate button')
        .to.not.exist;
      done();
    });
  });
});
