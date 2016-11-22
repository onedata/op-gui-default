/* jshint expr:true */
import { expect } from 'chai';
import sinon from 'sinon';
import sinonChai from 'op-worker-gui/hacks/sinon-chai';
import chai from 'chai';
chai.use(sinonChai);

import {
  describeComponent,
  it
} from 'ember-mocha';

import Ember from 'ember';
import ACE from 'op-worker-gui/utils/access-control-entity';
import { rejectPromise } from 'op-worker-gui/tests/test-helper';

import { POSIX_SPECIAL_DIFFERENT } from 'op-worker-gui/components/file-permissions/posix';

import { mockI18n } from 'ember-i18n-test-helpers';

describeComponent(
  'modals/file-permissions',
  'ModalsFilePermissionsComponent',
  {
    // Specify the other units that are required for this test
    needs: ['service:i18n'],
    unit: true,
    setup() {
      mockI18n().withDefault('text');
    }
  },
  function() {
    it('sets permissionsType to ACL and sets aclCache when every file has ACL permissions', function() {
      const component = this.subject();
      // fake FileAcl list
      let filesPermissions = [
        Ember.Object.create({
          type: 'acl',
          aclValue: Ember.A([ACE.create({subject: 'user', user: 'u1'})])
        }),
        Ember.Object.create({
          type: 'acl',
          aclValue: Ember.A([ACE.create({subject: 'group', user: 'g2'})])
        })
      ];

      component.handleResolvedPermissions(filesPermissions);

      expect(component.get('permissionsType')).to.be.equal('a');
    });

    it('sets permissionsType to POSIX when no File has FileACL using handleResolvedPermissions', function() {
      const component = this.subject();
      // fake FileAcl list
      const permissions = [
        Ember.Object.create({
          type: 'posix',
          posixValue: 777
        }),
        Ember.Object.create({
          type: 'posix',
          posixValue: 777
        })
      ];

      component.handleResolvedPermissions(permissions);

      expect(component.get('permissionsType')).to.be.equal('p');
    });

    it('sets permissionsType to mixed when not all files has acl type using handleResolvedPermissions', function() {
      const component = this.subject();
      // fake FileAcl list
      const permissions = [
        Ember.Object.create({
          type: 'acl',
          aclValue: Ember.A()
        }),
        Ember.Object.create({
          type: 'posix',
          posixValue: 644
        }),
      ];

      component.handleResolvedPermissions(permissions);

      expect(component.get('permissionsType')).to.be.equal('m');
    });

    it('sets statusBlocked to true when changed permissionsType to mixed', function() {
      const component = this.subject();

      component.set('permissionsType', 'm');

      expect(component.get('statusBlocked')).to.true;
    });

    it('sets all files permissions to posixCache in submitPosix method', function() {
      const component = this.subject();
      const posixValue = 777;
      const file1 = Ember.Object.create({id: '1', name: '2', save: function(){}});
      const filePermission1 = Ember.Object.create({id: '1', deleteRecord: function(){}, save: function(){}});
      const files = [
        file1
      ];
      const filePermissions = [
        filePermission1
      ];

      component.set('posixCache', posixValue);
      component.set('files', files);
      component.set('permissions', filePermissions);

      let saveFilePermission1 = sinon.stub(filePermission1, 'save', () => rejectPromise);

      component.submitPosix();

      expect(saveFilePermission1).to.have.been.calledOnce;
      expect(filePermission1.get('posixValue')).to.be.equal(posixValue);

      saveFilePermission1.restore();
    });

    it('updatePosixCache should set POSIX_SPECIAL_DIFFERENT code when posixes are different', function() {
      const comp = this.subject();

      let permissions = Ember.A([
        Ember.Object.create({
          posixValue: 644
        }),
        Ember.Object.create({
          posixValue: 644
        }),
        Ember.Object.create({
          posixValue: 777
        }),
      ]);
      comp.setProperties({
        permissions: permissions
      });

      comp.updatePosixCache();

      expect(comp.get('posixCache')).to.be.equal(POSIX_SPECIAL_DIFFERENT);
    });

    it('updatePosixCache should set common code when all posixes are same', function() {
      const comp = this.subject();
      const P_CODE = 644;

      const perms = Ember.A([
        Ember.Object.create({
          posixValue: P_CODE
        }),
        Ember.Object.create({
          posixValue: P_CODE
        }),
        Ember.Object.create({
          posixValue: P_CODE
        }),
      ]);
      comp.setProperties({
        permissions: perms
      });

      comp.updatePosixCache();

      expect(comp.get('posixCache')).to.be.equal(P_CODE);
    });

  }
);
