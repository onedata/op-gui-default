/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'modals/file-permissions/posix',
  'Integration: ModalsFilePermissionsPosixComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#modals/file-permissions/posix}}
      //     template content
      //   {{/modals/file-permissions/posix}}
      // `);

      this.render(hbs`{{modals/file-permissions/posix}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
