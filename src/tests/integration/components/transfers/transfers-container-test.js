import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | transfers/transfers container', function() {
  setupComponentTest('transfers/transfers-container', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#transfers/transfers-container}}
    //     template content
    //   {{/transfers/transfers-container}}
    // `);

    this.render(hbs`{{transfers/transfers-container}}`);
    expect(this.$()).to.have.length(1);
  });
});
