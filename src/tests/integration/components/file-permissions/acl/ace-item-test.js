/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration: FilePermissionsAclAceItemComponent', function() {
  setupComponentTest('file-permissions/acl/ace-item', {
    integration: true
  });

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
});
