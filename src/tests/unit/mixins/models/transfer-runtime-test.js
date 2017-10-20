import { expect } from 'chai';
import { describe, it } from 'mocha';
import Ember from 'ember';
import ModelsTransferRuntimeMixin from 'op-worker-gui/mixins/models/transfer-runtime';

const {
  Object: EmberObject,
  get,
  isArray,
} = Ember;

describe('Unit | Mixin | models/transfer runtime', function() {
  it('has sources computed property with collection of source provider ids', function() {
    let ModelsTransferRuntimeObject = EmberObject.extend(ModelsTransferRuntimeMixin);
    let subject = ModelsTransferRuntimeObject.create({
      stats: {
        hour: {
          provider1: [1, 2, 3],
          provider2: [2, 4, 5],
        },
        day: {
          provider1: [10, 20, 30],
          provider2: [20, 40, 50],
        },
        month: {
          provider1: [100, 200, 300],
          provider2: [200, 400, 500],
        }
      }
    });
    const sources = get(subject, 'sources');
    expect(isArray(sources)).to.be.true;
    expect(sources).to.have.lengthOf(2);
    expect(sources).to.contain('provider1');
    expect(sources).to.contain('provider2');
  });
});
