import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import wait from 'ember-test-helpers/wait';
import { click } from 'ember-native-dom-helpers';
import { registerService } from '../../../../helpers/stub-service';
import Ember from 'ember';
import sinon from 'sinon';

const {
  Service,
  RSVP: { resolve },
} = Ember;

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

const i18n = Service.extend({
  t() {
    return '';
  }
});

describe('Integration | Component | modals/file chunks/provider row', function () {
  setupComponentTest('modals/file-chunks/provider-row', {
    integration: true
  });
  
  beforeEach(function () {
    registerService(this, 'i18n', i18n);
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
      hasBlocksToInvalidate=true
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnMigrate = $providerRow.find('.btn-migrate');
      const $btnInvalidate = $providerRow.find('.btn-invalidate');
      const $btnReplicate = $providerRow.find('.btn-replicate');
            
      expect($btnMigrate).to.be.visible;
      expect($btnInvalidate).to.be.visible;
      expect($btnReplicate).to.be.visible;
      expect($btnMigrate, 'btn-migrate').to.have.class('disabled');
      expect($btnInvalidate, 'btn-invalidate').to.have.class('disabled');
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
      hasBlocksToInvalidate=true
    }}`);
    
    wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnMigrate = $providerRow.find('.btn-migrate');
      const $btnInvalidate = $providerRow.find('.btn-invalidate');
      const $btnReplicate = $providerRow.find('.btn-replicate');
      
      expect($btnMigrate).to.be.visible;
      expect($btnInvalidate).to.be.visible;
      expect($btnReplicate).to.be.visible;
      expect($btnMigrate, 'btn-migrate').to.not.have.class('disabled');
      expect($btnInvalidate, 'btn-invalidate').to.not.have.class('disabled');
      expect($btnReplicate, 'btn-replicate').to.not.have.class('disabled');
      
      done();
    });
  });
  
  it('allows to click invalidate even if invalidation is in progress', function () {
    const startInvalidation = sinon.spy();
    
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
      startInvalidation,
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      fileTransfers=fileTransfers
      transferEnabled=true
      hasBlocksToInvalidate=true
      invalidationInProgress=true
      startInvalidation=startInvalidation
    }}`);  
    
    return wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnInvalidate = $providerRow.find('.btn-invalidate');
      
      expect($btnInvalidate).to.be.visible;
      expect($btnInvalidate).to.not.have.class('disabled');
      
      click($btnInvalidate[0]).then(() => {
        expect(startInvalidation).to.have.been.calledOnce;
      });
    });
  });
  
  it('allows to click invalidate even if replication is in progress', function () {
    const startInvalidation = sinon.stub().returns(resolve());
    
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
      startInvalidation,
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      fileTransfers=fileTransfers
      transferEnabled=true
      hasBlocksToInvalidate=true
      replicationInProgress=true
      startInvalidation=startInvalidation
    }}`);  
    
    return wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnInvalidate = $providerRow.find('.btn-invalidate');
      
      expect($btnInvalidate).to.be.visible;
      expect($btnInvalidate).to.not.have.class('disabled');
      
      click($btnInvalidate[0]).then(() => {
        expect(startInvalidation).to.have.been.calledOnce;
      });
    });
  });
  
  it('allows to click migrate even if replication is in progress', function () {
    const openMigrationOptions = sinon.stub().returns(resolve());
    
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
      openMigrationOptions,
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      fileTransfers=fileTransfers
      transferEnabled=true
      hasBlocksToInvalidate=true
      replicationInProgress=true
      openMigrationOptions=openMigrationOptions
    }}`);  
    
    return wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnMigrate = $providerRow.find('.btn-migrate');
      
      expect($btnMigrate).to.be.visible;
      expect($btnMigrate).to.not.have.class('disabled');
      
      click($btnMigrate[0]).then(() => {
        expect(openMigrationOptions).to.have.been.calledOnce;
      });
    });
  });
  
  it('allows to click replicate even if migration is in progress', function () {
    const startReplication = sinon.stub().returns(resolve());
    
    this.setProperties({
      fileDistribution: FILE_DISTRIBUTION_F1_NS,
      fileTransfers: [],
      startReplication,
    });
    
    this.render(hbs`{{modals/file-chunks/provider-row
      fileDistribution=fileDistribution
      fileTransfers=fileTransfers
      transferEnabled=true
      hasBlocksToInvalidate=true
      replicationInProgress=true
      startReplication=startReplication
    }}`);  
    
    return wait().then(() => {
      const $providerRow = this.$('.provider-row');
      const $btnReplicate = $providerRow.find('.btn-replicate');
      
      expect($btnReplicate).to.be.visible;
      expect($btnReplicate).to.not.have.class('disabled');
      
      click($btnReplicate[0]).then(() => {
        expect(startReplication).to.have.been.calledOnce;
      });
    });
  });
});
