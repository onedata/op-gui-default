import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | one overlay', function() {
  setupComponentTest('one-overlay', {
    integration: true
  });

  // FIXME real tests

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#one-overlay}}
    //     template content
    //   {{/one-overlay}}
    // `);

    this.render(hbs`{{one-overlay}}`);
    expect(this.$()).to.have.length(1);
  });
});
