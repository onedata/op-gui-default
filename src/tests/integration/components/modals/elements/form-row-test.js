/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration: ModalsElementsFormRowComponent', function() {
  setupComponentTest('modals/elements/form-row', {
    integration: true
  });

  it('renders', function() {
    // Set any properties with this.set('myProperty', 'value');
    // Handle any actions with this.on('myAction', function(val) { ... });
    // Template block usage:
    // this.render(hbs`
    //   {{#modals/elements/form-row}}
    //     template content
    //   {{/modals/elements/form-row}}
    // `);

    this.render(hbs`{{modals/elements/form-row}}`);
    expect(this.$()).to.have.length(1);
  });
});
