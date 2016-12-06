/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import Ember from 'ember';
import ComponentsModalMixin from 'ember-cli-onedata-common/mixins/components/modal';

describe('ComponentsModalMixin', function() {
  // Replace this with your real tests.
  it('works', function() {
    let ComponentsModalObject = Ember.Object.extend(ComponentsModalMixin);
    let subject = ComponentsModalObject.create();
    expect(subject).to.be.ok;
  });
});
