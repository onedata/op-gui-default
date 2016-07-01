/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'modals/file-permissions',
  'Integration: ModalsFilePermissionsComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#modals/file-permissions}}
      //     template content
      //   {{/modals/file-permissions}}
      // `);

      this.render(hbs`{{modals/file-permissions}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
