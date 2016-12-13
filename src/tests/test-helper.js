import resolver from './helpers/resolver';
import { setResolver } from 'ember-mocha';
import Ember from 'ember';

setResolver(resolver);

const resolvePromise = new Ember.RSVP.Promise((resolve) => {
  resolve();
});

const rejectPromise = new Ember.RSVP.Promise((resolve, reject) => {
  reject();
});

export { resolvePromise, rejectPromise };
