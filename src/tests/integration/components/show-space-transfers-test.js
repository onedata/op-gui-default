import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import Ember from 'ember';
import { registerService } from '../../helpers/stub-service';
import mockHasMany from 'op-worker-gui/utils/mock-has-many';
import wait from 'ember-test-helpers/wait';

const {
  Service,
} = Ember;

const PROVIDER_ID = 'p1';

const SessionStub = Service.extend({
  sessionDetails: {
    providerId: PROVIDER_ID,
  }
});

describe('Integration | Component | show space transfers', function() {
  setupComponentTest('show-space-transfers', {
    integration: true
  });

  beforeEach(function () {
    registerService(this, 'session', SessionStub);
  });
  
  it('renders warning message if space is not supported by session provider', function(done) {
    const space = {
      id: 's1',
      providerList: {
        queryList: mockHasMany(['p2', 'p3'])
      }
    };
    this.set('space', space);
    
    this.render(hbs`{{show-space-transfers space=space}}`);
    
    wait().then(() => {
      expect(this.$('.error-space-not-supported'), 'error message renders')
        .to.exist;
      done();
    });
  });
  
  it('renders loading spinner if space support by provider is not yet defined', function(done) {
    const space = {
      id: 's1',
      providerList: {
        queryList: mockHasMany(['p2', 'p3'])
      }
    };
    this.set('space', space);
    
    this.render(hbs`{{show-space-transfers space=space}}`);
    
    expect(this.$('.row-transfers-data-container > .spin-spinner-block'), 'error message renders')
      .to.exist;
    done();
  });
});
