/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'one-option',
  'Integration: OneOptionComponent',
  {
    integration: true
  },
  function() {
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
  }
);
