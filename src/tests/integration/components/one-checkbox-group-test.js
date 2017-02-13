/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration: OneCheckboxGroupComponent', function() {
  setupComponentTest('one-checkbox-group', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#one-checkbox-group}}
    //     template content
    //   {{/one-checkbox-group}}
    // `);

    this.render(hbs`{{one-checkbox-group}}`);
    expect(this.$()).to.have.length(1);
  });
});
