/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration: OneCheckboxButtonComponent', function() {
  setupComponentTest('one-checkbox-button', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#one-checkbox-button}}
    //     template content
    //   {{/one-checkbox-button}}
    // `);

    this.render(hbs`{{one-checkbox-button}}`);
    expect(this.$()).to.have.length(1);
  });
});
