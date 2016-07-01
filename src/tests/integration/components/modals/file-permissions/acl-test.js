/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'modals/file-permissions/acl',
  'Integration: ModalsFilePermissionsAclComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#modals/file-permissions/acl}}
      //     template content
      //   {{/modals/file-permissions/acl}}
      // `);

      this.render(hbs`{{modals/file-permissions/acl}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
