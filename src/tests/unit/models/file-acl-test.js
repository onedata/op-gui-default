/* jshint expr:true */
import { expect } from 'chai';
import {
  describeModel,
  it
} from 'ember-mocha';

describeModel(
  'file-acl',
  'FileAcl',
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
  }
);
