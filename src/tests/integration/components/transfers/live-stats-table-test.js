import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | transfers/live stats table', function() {
  setupComponentTest('transfers/live-stats-table', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#transfers/live-stats-table}}
    //     template content
    //   {{/transfers/live-stats-table}}
    // `);

    this.render(hbs`{{transfers/live-stats-table}}`);
    expect(this.$()).to.have.length(1);
  });
});
