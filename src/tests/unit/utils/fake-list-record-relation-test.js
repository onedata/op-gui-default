import { expect } from 'chai';
import { describe, it } from 'mocha';
import FakeListRecordRelation from 'op-worker-gui/utils/fake-list-record-relation';
import Ember from 'ember';
import wait from 'ember-test-helpers/wait';
const {
  get,
  RSVP: { Promise },
} = Ember;

describe('Unit | Utility | utils/fake list record relation', function () {
  it('requires to be created with initChunkArray', function () {
    expect(function () {
      FakeListRecordRelation.create({});
    }).to.throw();
  });

  it('has isLoading flag until initialLoad is completed', function () {
    const initChunksArray = {
      initialLoad: Promise.resolve('test'),
    };

    const relation = FakeListRecordRelation.create({
      initChunksArray,
    });

    expect(get(relation, 'isLoading'), 'isLoading before resolve')
      .to.be.true;

    return wait().then(() => {
      expect(get(relation, 'isLoading'), 'isLoading after resolve')
        .to.be.false;
    });
  });
});
