/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'truncated-string',
  'Integration: TruncatedStringComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#truncated-string}}
      //     template content
      //   {{/truncated-string}}
      // `);

      this.render(hbs`{{truncated-string}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
