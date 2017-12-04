import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import wait from 'ember-test-helpers/wait';
import hbs from 'htmlbars-inline-precompile';
import sinon from 'sinon';

const PROVIDER = {
  id: 'p1',
  name: 'Provider One',
};

describe('Integration | Component | modals/file chunks/migrate popover/migrate item', function() {
  setupComponentTest('modals/file-chunks/migrate-popover/migrate-item', {
    integration: true
  });

  it('renders provider name and with class containing provider Id', function() {
    this.setProperties({
      providerId: PROVIDER.id,
      providerName: PROVIDER.name,
    });
    
    this.render(hbs`{{modals/file-chunks/migrate-popover/migrate-item
      providerId=providerId
      providerName=providerName
    }}`);
    
    const $migrateItem = this.$('.migrate-item');
    expect($migrateItem).to.exist;
    expect($migrateItem.attr('class')).to.match(new RegExp(`.*${PROVIDER.id}.*`));
    expect($migrateItem.text()).to.match(new RegExp(PROVIDER.name));
  });
  
  it('invokes passed action on click with provider id', function(done) {
    const spyAction = sinon.spy();
    this.setProperties({
      providerId: PROVIDER.id,
      providerName: PROVIDER.name,
    });
    this.on('spyAction', spyAction);
    
    this.render(hbs`{{modals/file-chunks/migrate-popover/migrate-item
      providerId=providerId
      providerName=providerName
      action=(action "spyAction")
    }}`);
    
    $('.migrate-item').click();
    wait().then(() => {
      expect(spyAction).to.be.calledOnce;
      expect(spyAction).to.be.calledWith(PROVIDER.id);
      done();
    });
  });
  
  it('does not allow to click on disabled item', function(done) {
    const spyAction = sinon.spy();
    this.setProperties({
      providerId: PROVIDER.id,
      providerName: PROVIDER.name,
    });
    this.on('spyAction', spyAction);
    
    this.render(hbs`{{modals/file-chunks/migrate-popover/migrate-item
      providerId=providerId
      providerName=providerName
      action=(action "spyAction")
      disabled=true
    }}`);
    
    $('.migrate-item').click();
    wait().then(() => {
      expect(spyAction).to.not.be.called;
      done();
    });
  });
  
  it('is disabled and renders busy tag on disabled item', function() {
    const busyText = 'busy';
    this.setProperties({
      providerId: PROVIDER.id,
      providerName: PROVIDER.name,
    });
    
    this.render(hbs`{{modals/file-chunks/migrate-popover/migrate-item
      providerId=providerId
      providerName=providerName
      disabled=true
    }}`);
    
    const $migrateItem = this.$('.migrate-item');
    expect($migrateItem, 'migrate item').to.exist;
    expect($migrateItem, 'migrate item').to.have.class('disabled');
    expect($migrateItem.attr('class')).to.match(new RegExp(`.*${PROVIDER.id}.*`));
    expect($migrateItem.text(), 'migrate item text').to.match(new RegExp(busyText));
  });
});
