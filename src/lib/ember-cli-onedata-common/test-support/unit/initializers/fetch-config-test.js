/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it,
  beforeEach
} from 'mocha';
import Ember from 'ember';
import { initialize } from 'op-worker-gui/initializers/fetch-config';

describe('FetchConfigInitializer', function() {
  let application;

  beforeEach(function() {
    Ember.run(function() {
      application = Ember.Application.create();
      application.deferReadiness();
    });
  });

  it('adds a getOnedataConfig method to application', function() {
    initialize(application);

    expect(typeof(application.getOnedataConfig)).to.be.equal('function');
  });

  it('creates application.getOnedataConfig method which resolves with an object', function(done) {
    initialize(application);

    let configPromise = application.getOnedataConfig();

    configPromise.then(config => {
      expect(typeof(config)).to.be.equal('object');
    });
    configPromise.catch(() => {
      expect(false, 'getOnedataConfig promise rejected').to.be.ok;
    });
    configPromise.finally(done);
  });
});
