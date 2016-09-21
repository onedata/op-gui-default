/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'data-files-list/file-metadata-row',
  'Integration: DataFilesListFileMetadataRowComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#data-files-list/file-metadata-row}}
      //     template content
      //   {{/data-files-list/file-metadata-row}}
      // `);

      this.render(hbs`{{data-files-list/file-metadata-row}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
