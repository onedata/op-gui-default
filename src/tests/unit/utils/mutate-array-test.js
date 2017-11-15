import { expect } from 'chai';
import { describe, it } from 'mocha';
import Ember from 'ember';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import sinon from 'sinon';

import _ from 'lodash';

const {
  A,
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
});
