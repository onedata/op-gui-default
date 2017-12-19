import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | loading container', function () {
  setupComponentTest('loading-container', {
    integration: true
  });

  it('renders yielded content if isLoading is false', function () {
    this.render(hbs `{{#loading-container isLoading=false}}
      <div class="some-content">Some content</div>
    {{/loading-container}}
    `);
    expect(this.$('.some-content')).to.exist;
  });

  it('does not render yielded content if isLoading is true', function () {
    this.render(hbs `{{#loading-container isLoading=true}}
      <div class="some-content">Some content</div>
    {{/loading-container}}
    `);
    expect(this.$('.some-content')).to.not.exist;
  });

  it('render erroReason if available', function () {
    this.render(hbs `{{#loading-container errorReason="some reason"}}
      <div class="some-content">Some content</div>
    {{/loading-container}}
    `);
    expect(this.$('.some-content')).to.not.exist;
    expect(this.$('.resource-load-error')).to.contain('some reason');
  });
});
