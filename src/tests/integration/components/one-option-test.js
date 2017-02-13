/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration: OneOptionComponent', function() {
  setupComponentTest('one-option', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#one-option}}
    //     template content
    //   {{/one-option}}
    // `);

    this.render(hbs`{{one-option}}`);
    expect(this.$()).to.have.length(1);
  });
});
