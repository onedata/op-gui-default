/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it,
  beforeEach
} from 'mocha';
import Ember from 'ember';
import { initialize } from 'op-worker-gui/instance-initializers/global-app';

describe('GlobalAppInstanceInitializer', function() {
  let appInstance;

  beforeEach(function() {
    Ember.run(function() {
      const application = Ember.Application.create();
      appInstance = application.buildInstance();
    });
  });

  it('exposes window.App variable which is a reference to appInstance', function() {
    initialize(appInstance);

    expect(window.App).to.be.equal(appInstance);
  });
});
