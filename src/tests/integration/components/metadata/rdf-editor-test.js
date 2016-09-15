/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'metadata/rdf-editor',
  'Integration: MetadataRdfEditorComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#metadata/rdf-editor}}
      //     template content
      //   {{/metadata/rdf-editor}}
      // `);

      this.render(hbs`{{metadata/rdf-editor}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
