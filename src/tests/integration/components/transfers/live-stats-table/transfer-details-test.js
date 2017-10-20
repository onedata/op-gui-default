import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | transfers/live stats table/transfer details', function() {
  setupComponentTest('transfers/live-stats-table/transfer-details', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#transfers/live-stats-table/transfer-details}}
    //     template content
    //   {{/transfers/live-stats-table/transfer-details}}
    // `);

    this.render(hbs`{{transfers/live-stats-table/transfer-details}}`);
    expect(this.$()).to.have.length(1);
  });
});
