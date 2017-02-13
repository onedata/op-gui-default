/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration: DataFilesListFileMetadataRowComponent', function() {
  setupComponentTest('data-files-list/file-metadata-row', {
    integration: true
  });

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
});
