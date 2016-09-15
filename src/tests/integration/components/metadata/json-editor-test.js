/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'metadata/json-editor',
  'Integration: MetadataJsonEditorComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#metadata/json-editor}}
      //     template content
      //   {{/metadata/json-editor}}
      // `);

      this.render(hbs`{{metadata/json-editor}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
