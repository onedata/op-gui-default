import Ember from 'ember';
import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';

const {
  RSVP: { Promise },
  A,
} = Ember;

export default function mockHasMany(array) {
  return PromiseArray.create({
    promise: Promise.resolve(A(array))
  });
}
