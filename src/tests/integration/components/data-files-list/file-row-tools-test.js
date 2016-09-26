/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'data-files-list/file-row-tools',
  'Integration: DataFilesListFileRowToolsComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#data-files-list/file-row-tools}}
      //     template content
      //   {{/data-files-list/file-row-tools}}
      // `);

      this.render(hbs`{{data-files-list/file-row-tools}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
