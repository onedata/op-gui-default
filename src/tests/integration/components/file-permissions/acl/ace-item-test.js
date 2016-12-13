/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'file-permissions/acl/ace-item',
  'Integration: FilePermissionsAclAceItemComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#file-permissions/acl/ace-item}}
      //     template content
      //   {{/file-permissions/acl/ace-item}}
      // `);

      this.render(hbs`{{file-permissions/acl/ace-item}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
