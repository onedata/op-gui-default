/* jshint expr:true */
import { expect } from 'chai';
import {
  describeModel,
  it
} from 'ember-mocha';

describeModel(
  'file-access-control',
  'FileAccessControl',
  {
    // Specify the other units that are required for this test.
      needs: ['model:file', 'model:system-user', 'model:system-group']
  },
  function() {
    // Replace this with your real tests.
    it('exists', function() {
      let model = this.subject();
      // var store = this.store();
      expect(model).to.be.ok;
    });

    it('after setting a permission with setPermission using type name, it is true checking with hasPermission', function() {
      let model = this.subject();
      const permissionType = 'execute';

      model.setPermission(permissionType);

      expect(model.hasPermission(permissionType)).to.be.true;
    });

    it('after unsetting a permission with unsetPermission using type name, it is false checking with hasPermission', function() {
      let model = this.subject();
      const permissionType = 'execute';

      model.setPermission(permissionType);
      model.unsetPermission(permissionType);

      expect(model.hasPermission(permissionType)).to.be.false;
    });

    it('setting some permissions should not affect unset permissions', function() {
      let model = this.subject();


      model.setPermission('read_acl');
      model.unsetPermission('read_acl');
      model.setPermission('write_object');
      model.setPermission('delete');

      expect(model.hasPermission('read_acl')).to.be.false;
    });

    it('unsetting some permissions should not affect set permissions', function() {
      let model = this.subject();

      model.setPermission('write_object');
      model.setPermission('delete');
      model.setPermission('read_acl');
      model.unsetPermission('write_object');
      model.unsetPermission('delete');

      expect(model.hasPermission('read_acl')).to.be.true;
    });
  }
);
