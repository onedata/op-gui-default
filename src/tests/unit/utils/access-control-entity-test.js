/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import AccessControlEntity from 'op-worker-gui/utils/access-control-entity';

describe('accessControlEntity', function() {
  it('can be created with empty JSON', function() {
    let result = AccessControlEntity.create({});
    expect(result).to.be.not.null;
  });

  it('attributes can be read after creation with these attributes', function() {
    let acl = AccessControlEntity.create({
      permissions: 2,
      type: 'deny'
    });
    expect(acl.get('permissions')).to.be.equal(2);
    expect(acl.get('type')).to.be.equal('deny');
  });

  it('after setting a permission with setPermission using type name, it is true checking with hasPermission', function() {
    let ace = AccessControlEntity.create({});
    const permissionType = 'execute';

    ace.setPermission(permissionType);

    expect(ace.hasPermission(permissionType)).to.be.true;
  });

  it('after unsetting a permission with unsetPermission using type name, it is false checking with hasPermission', function() {
    let ace = AccessControlEntity.create({});
    const permissionType = 'execute';

    ace.setPermission(permissionType);
    ace.setPermission(permissionType, false);

    expect(ace.hasPermission(permissionType)).to.be.false;
  });

  it('setting some permissions should not affect unset permissions', function() {
    let ace = AccessControlEntity.create({});

    ace.setPermission('read_acl');
    ace.setPermission('read_acl', false);
    ace.setPermission('write_object');
    ace.setPermission('delete');

    expect(ace.hasPermission('read_acl')).to.be.false;
  });

  it('unsetting some permissions should not affect set permissions', function() {
    let ace = AccessControlEntity.create({});

    ace.setPermission('write_object');
    ace.setPermission('delete');
    ace.setPermission('read_acl');
    ace.setPermission('write_object', false);
    ace.setPermission('delete', false);

    expect(ace.hasPermission('read_acl')).to.be.true;
  });

  it('permission can be set using computed property', function() {
    let ace = AccessControlEntity.create({
      permissions: 0
    });

    expect(ace.get('perm_write_owner')).to.be.false;

    ace.set('perm_write_owner', true);

    expect(ace.get('perm_write_owner')).to.be.true;
  });
});
