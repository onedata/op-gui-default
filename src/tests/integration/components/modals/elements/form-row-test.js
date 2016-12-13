/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'modals/elements/form-row',
  'Integration: ModalsElementsFormRowComponent',
  {
    integration: true
  },
  function() {
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
  }
);
