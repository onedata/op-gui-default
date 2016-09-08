/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'metadata-panel',
  'Integration: MetadataPanelComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#metadata-panel}}
      //     template content
      //   {{/metadata-panel}}
      // `);

      this.render(hbs`{{metadata-panel}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
