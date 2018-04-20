import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | loading more spinner', function() {
  setupComponentTest('loading-more-spinner', {
    integration: true
  });

  it('renders with spin-spiner and spinner classes', function() {
    this.render(hbs`{{loading-more-spinner}}`);
    const $loadingMoreSpinner = this.$('.spinner-container');
    expect($loadingMoreSpinner).to.have.class('spinner-64');
    expect($loadingMoreSpinner).to.have.class('spinner-centered');
    expect($loadingMoreSpinner.find('.spin-spinner')).to.exist;
  });
});
