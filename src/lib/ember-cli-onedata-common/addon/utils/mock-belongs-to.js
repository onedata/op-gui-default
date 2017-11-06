import Ember from 'ember';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  RSVP: { Promise },
} = Ember;


export default function mockBelongsTo(obj) {
  return PromiseObject.create({
    promise: Promise.resolve(obj),
  });
}
