import { expect } from 'chai';
import { describe, it } from 'mocha';
import Ember from 'ember';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import sinon from 'sinon';

import _ from 'lodash';

const {
  get,
  A,
  Object: EmberObject,
} = Ember;

function createArray(type, content) {
  switch (type) {
    case 'ember':
      return A(content);
    case 'plain':
      return content;
    default:
      throw `Unsupported array type: ${type}`;
  }
}

describe('Unit | Utility | mutate array', function () {

  ['plain', 'ember'].forEach(arrayType => {
    it(`adds and removes items using compare fun. for ${arrayType} array`,
      function () {
        const a2 = { a: 2 };
        const orig = createArray(arrayType, [
          { a: 1 },
          a2,
          { a: 3, b: 'x' },
          { a: 99 },
        ]);
        const update = createArray(arrayType, [
          { a: 2 },
          { a: 3, b: 'y', c: 'z' },
          { a: 4, b: 'x' },
        ]);

        const result = mutateArray(orig, update, (x, y) => x.a === y.a);

        expect(result).to.equal(orig);
        expect(result).to.have.lengthOf(3);

        expect(_.find(result, { a: 2 }), 'do not replace reference')
          .to.equal(a2);
        expect(_.find(result, { a: 3 })).to.deep.equal({ a: 3, b: 'y', c: 'z' });
        expect(_.find(result, { a: 4 })).to.deep.equal({ a: 4, b: 'x' });
      });
  });

  it('uses pushObject on EmberArrays', function () {
    const orig = A();
    const pushObject = sinon.spy(orig, 'pushObject');

    mutateArray(orig, ['test'], (x, y) => x === y);

    expect(pushObject).to.be.calledOnce;
  });
  
  it('mutates ember objects in array', function () {
    const origObj = EmberObject.create({
      a: 1,
      b: 2,
    });
    const updateObj = EmberObject.create({
      a: 1,
      b: 3,
    });    
    const orig = createArray('plain', [
      origObj,
    ]);
    const updated = createArray('plain', [
      updateObj,
    ]);
    mutateArray(orig, updated, (x, y) => get(x, 'a') === get(y, 'a'), false);
    
    expect(orig).to.have.lengthOf(1);
    expect(orig[0]).to.equal(origObj);
    expect(get(orig[0], 'b')).to.equal(3);
  });
});
