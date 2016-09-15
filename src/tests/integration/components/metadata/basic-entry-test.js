/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'metadata/basic-entry',
  'Integration: MetadataBasicEntryComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#metadata/basic-entry}}
      //     template content
      //   {{/metadata/basic-entry}}
      // `);

      this.render(hbs`{{metadata/basic-entry}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
