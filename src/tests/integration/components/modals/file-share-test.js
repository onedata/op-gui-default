/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'modals/file-share',
  'Integration: ModalsFileShareComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#modals/file-share}}
      //     template content
      //   {{/modals/file-share}}
      // `);

      this.render(hbs`{{modals/file-share}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
