import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | transfers/providers map', function() {
  setupComponentTest('transfers/providers-map', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#transfers/providers-map}}
    //     template content
    //   {{/transfers/providers-map}}
    // `);

    this.render(hbs`{{transfers/providers-map}}`);
    expect(this.$()).to.have.length(1);
  });
});
