import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | transfers/throughput distribution', function() {
  setupComponentTest('transfers/throughput-distribution', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#transfers/throughput-distribution}}
    //     template content
    //   {{/transfers/throughput-distribution}}
    // `);

    this.render(hbs`{{transfers/throughput-distribution}}`);
    expect(this.$()).to.have.length(1);
  });
});
