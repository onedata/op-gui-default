/* jshint expr:true */
import { expect } from 'chai';
import { setupTest } from 'ember-mocha';
import {
  beforeEach,
  afterEach,
  it,
  describe
} from 'mocha';

import startApp from 'op-worker-gui/tests/helpers/start-app';

import Ember from 'ember';

describe('Integration: SecondaryMenuService', function() {
  setupTest('service:secondary-menu', {
    integration: true
  });

  beforeEach(function () {
    this.application = startApp();
    this.store = this.application.__container__.lookup('service:store');
  });

  afterEach(function() {
    Ember.run(this.application, 'destroy');
  });

  it('exists', function() {
    let service = this.subject();
    expect(service).to.be.ok;
  });

  it('returns activeSpace if activeItem is of Space type', function () {
    let g = this.store.createRecord('space', {id: 's1'});

    this.subject().set('activeItem', g);

    expect(this.subject().get('activeSpace')).to.not.be.null;
    expect(this.subject().get('activeSpace.id')).to.equal('s1');
  });

  it('returns null activeShare if activeItem is of Space type', function () {
    let s = this.store.createRecord('space', {id: 's1'});

    this.subject().set('activeItem', s);

    expect(this.subject().get('activeShare')).to.be.null;
  });
});
