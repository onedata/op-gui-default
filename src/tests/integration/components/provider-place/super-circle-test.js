import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | provider place/super circle', function() {
  setupComponentTest('provider-place/super-circle', {
    integration: true
  });

  it('renders with source animation class', function() {
    this.render(hbs`
{{provider-place/super-circle isSource=true}}
    `);
    
    const $superCircle = this.$('.super-circle');
        
    expect($superCircle).to.have.class('source');
  });
  
  it('renders with destination animation class', function() {
    this.render(hbs`
{{provider-place/super-circle isDestination=true}}
    `);
    
    const $superCircle = this.$('.super-circle');
        
    expect($superCircle).to.have.class('destination');
  });
  
  it('renders with source and destination animation classes', function() {
    this.render(hbs`
{{provider-place/super-circle isDestination=true isSource=true}}
    `);
    
    const $superCircle = this.$('.super-circle');
    
    expect($superCircle).to.have.class('source');
    expect($superCircle).to.have.class('destination');
  });
});
