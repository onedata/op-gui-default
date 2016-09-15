/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'metadata/basic-editor',
  'Integration: MetadataBasicEditorComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#metadata/basic-editor}}
      //     template content
      //   {{/metadata/basic-editor}}
      // `);

      this.render(hbs`{{metadata/basic-editor}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
