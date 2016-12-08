/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'one-radio-button',
  'Integration: OneRadioButtonComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#one-radio-button}}
      //     template content
      //   {{/one-radio-button}}
      // `);

      this.render(hbs`{{one-radio-button}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
