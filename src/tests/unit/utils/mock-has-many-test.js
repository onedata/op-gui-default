import { expect } from 'chai';
import { describe, it } from 'mocha';
import mockHasMany from 'op-worker-gui/utils/mock-has-many';

import Ember from 'ember';

const {
  Object: EmberObject,
  isArray,
} = Ember;

describe('Unit | Utility | mock has many', function () {
  it('behaves like hasMany relation', function (done) {
    const obj = EmberObject.create({
      someList: mockHasMany([0, 1, 2]),
    });
    obj.get('someList').then(result => {
      expect(isArray(result)).to.be.true;
      expect(result, 'is Ember Array').to.have.property('objectAt');
      [0, 1, 2].forEach(i => expect(result.objectAt(i)).to.equal(i));
      done();
    });
  });
});
