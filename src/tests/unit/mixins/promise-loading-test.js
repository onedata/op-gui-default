/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';

describe('PromiseLoadingMixin', function() {
  // Replace this with your real tests.
  it('works', function() {
    let PromiseLoadingObject = Ember.Object.extend(PromiseLoadingMixin);
    let subject = PromiseLoadingObject.create();
    expect(subject).to.be.ok;
  });
});
