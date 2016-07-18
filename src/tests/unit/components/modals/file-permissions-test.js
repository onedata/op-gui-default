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
import ACE from '../../../../utils/access-control-entity';
import { resolvePromise, rejectPromise } from '../../../test-helper';

import { POSIX_SPECIAL_DIFFERENT } from '../../../../components/file-permissions/posix';

describeComponent(
  'modals/file-permissions',
  'ModalsFilePermissionsComponent',
  {
    // Specify the other units that are required for this test
    // needs: ['component:foo', 'helper:bar'],
    unit: true
  },
  function() {
    it('sets permissionsType to ACL and sets aclCache when every ACL is non-empty using handleResolvedAcls', function() {
      const component = this.subject();
      // fake FileAcl list
      const acls = [
        Ember.Object.create({
          acl: Ember.A([ACE.create({subject: 'user', user: 'u1'})])
        }),
        Ember.Object.create({
          acl: Ember.A([ACE.create({subject: 'group', user: 'g2'})])
        })
      ];

      component.handleResolvedAcls(acls);

      expect(component.get('permissionsType')).to.be.equal('a');
    });

    it('sets permissionsType to POSIX when no File has FileACL using handleResolvedAcls', function() {
      const component = this.subject();
      // fake FileAcl list
      const acls = [
        null,
        null,
        null
      ];

      component.handleResolvedAcls(acls);

      expect(component.get('permissionsType')).to.be.equal('p');
    });

    it('sets permissionsType to mixed when not all files has FileACL using handleResolvedAcls', function() {
      const component = this.subject();
      // fake FileAcl list
      const acls = [
        Ember.Object.create({
          acl: Ember.A([ACE.create({subject: 'user', user: 'u1'})])
        }),
        null,
        null
      ];

      component.handleResolvedAcls(acls);

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
      const fileAcl1 = Ember.Object.create({id: '1', deleteRecord: function(){}, save: function(){}});
      const files = [
        file1
      ];
      const fileAcls = [
        fileAcl1
      ];
      const filesToFileAcl = new Map();
      filesToFileAcl.set(files[0], fileAcls[0]);

      component.set('posixCache', posixValue);
      component.set('files', files);
      component.set('filesToFileAcl', filesToFileAcl);

      let saveFile1 = sinon.stub(file1, 'save', () => resolvePromise);
      let deleteFileAcl1 = sinon.stub(fileAcl1, 'deleteRecord');
      let saveFileAcl1 = sinon.stub(fileAcl1, 'save', () => rejectPromise);

      component.submitPosix();

      expect(saveFile1).to.have.been.calledOnce;
      expect(deleteFileAcl1).to.have.been.calledOnce;
      expect(saveFileAcl1).to.have.been.calledOnce;
      expect(file1.get('permissions')).to.be.equal(posixValue);

      saveFile1.restore();
      deleteFileAcl1.restore();
      saveFileAcl1.restore();
    });

    it('updatePosixCache should set POSIX_SPECIAL_DIFFERENT code when posixes are different', function() {
      const comp = this.subject();

      const files = Ember.A([
        Ember.Object.create({
          permissions: 644
        }),
        Ember.Object.create({
          permissions: 644
        }),
        Ember.Object.create({
          permissions: 777
        }),
      ]);
      comp.setProperties({
        files: files
      });

      comp.updatePosixCache();

      expect(comp.get('posixCache')).to.be.equal(POSIX_SPECIAL_DIFFERENT);
    });

    it('updatePosixCache should set common code when all posixes are same', function() {
      const comp = this.subject();
      const P_CODE = 644;

      const files = Ember.A([
        Ember.Object.create({
          permissions: P_CODE
        }),
        Ember.Object.create({
          permissions: P_CODE
        }),
        Ember.Object.create({
          permissions: P_CODE
        }),
      ]);
      comp.setProperties({
        files: files
      });

      comp.updatePosixCache();

      expect(comp.get('posixCache')).to.be.equal(P_CODE);
    });

    it('updatePosixCache should detect POSIX equality after ACL to POSIX changes', function() {
      const P_CODE = 777;

      const comp = this.subject();
      const file1 = Ember.Object.create({save: function(){}});
      const file1Acl = Ember.Object.create();
      const filesToFileAcl = new Map();

      filesToFileAcl.set(file1, file1Acl);
      comp.set('posixCache', P_CODE);
      comp.set('files', [file1]);
      comp.submitPosix();

      const file2 = Ember.Object.create({
        permissions: P_CODE
      });
      // do not invoke observers
      comp.get('files').push(file2);

      comp.updatePosixCache();
      expect(comp.get('posixCache')).to.be.equal(P_CODE);
    });
  }
);
