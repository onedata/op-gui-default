/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration: BsTooltipComponent', function() {
  setupComponentTest('bs-tooltip', {
    integration: true
  });

  it('generates element with data-toggle and data-original-title', function() {
    this.render(hbs`
      {{#bs-tooltip class="test-bs-tooltip" title="Hello world"}}
        Something different
      {{/bs-tooltip}}
    `);
    let $tooltip = this.$().find('.test-bs-tooltip');
    expect($tooltip).to.have.attr('data-toggle', 'tooltip');
    expect($tooltip).to.have.attr('data-original-title', 'Hello world');
  });

  it('yields inner content without modifications', function() {
    this.render(hbs`
      {{#bs-tooltip class="test-bs-tooltip" title="Hello world"}}
        <div>inner content</div>
      {{/bs-tooltip}}
    `);
    let $tooltip = this.$().find('.test-bs-tooltip');
    expect($tooltip.html()).to.match(/\s*<div>inner content<\/div>\s*/);
  });
});
