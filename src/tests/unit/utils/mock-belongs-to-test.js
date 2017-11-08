import { expect } from 'chai';
import { describe, it } from 'mocha';
import mockBelongsTo from 'op-worker-gui/utils/mock-belongs-to';

import Ember from 'ember';

const {
  Object: EmberObject,
} = Ember;

describe('Unit | Utility | mock belongs to', function () {
  it('behaves like belongsTo relation', function (done) {
    const someObject = {};
    const obj = EmberObject.create({
      someRelation: mockBelongsTo(someObject),
    });
    obj.get('someRelation').then(result => {
      expect(result).to.equal(someObject);
      done();
    });
  });
});
