/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it,
  beforeEach,
  afterEach
} from 'mocha';
import Ember from 'ember';
import { initialize } from 'op-worker-gui/instance-initializers/global-app';
import startApp from 'op-worker-gui/tests/helpers/start-app';

describe('GlobalAppInstanceInitializer', function() {
  beforeEach(function () {
    this.application = startApp();
  });

  afterEach(function() {
    Ember.run(this.application, 'destroy');
  });

  it('exposes window.App variable which is a reference to appInstance', function() {
    initialize(this.application);

    expect(window.App).to.be.equal(this.application);
  });
});
