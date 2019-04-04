import { expect } from 'chai';
import { describe, it } from 'mocha';
import resolveSupportedResource from 'op-worker-gui/utils/resolve-supported-resource';
import Ember from 'ember';

const {
  RSVP: { resolve },
} = Ember;

describe('Unit | Utility | resolve supported resource', function () {
  it('resolves resource that has current provider on its supporters list', function () {
    const collection = [{
        id: 1,
        providers: resolve(['a', 'b', 'c'])
      },
      {
        id: 2,
        providers: resolve(['b', 'c'])
      },
      {
        id: 3,
        providers: resolve(['a', 'b', 'c'])
      },
      {
        id: 4,
        providers: resolve(['a', 'b', 'c', 'd'])
      },
      {
        id: 5,
        providers: resolve(['a', 'd'])
      },
      {
        id: 6,
        providers: resolve(['c'])
      }
    ];

    return resolveSupportedResource(
      collection,
      0,
      'd',
      item => item.providers
    ).then(record => {
      expect(record.id).to.be.equal(4);
    });
  });
});
