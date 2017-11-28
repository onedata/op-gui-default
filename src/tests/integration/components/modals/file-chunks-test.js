import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import sinon from 'sinon';
import Ember from 'ember';
import { registerService } from '../../../helpers/stub-service';

const {
  Service,
  Object: EmberObject,
  RSVP: { Promise },
} = Ember;

const FILE_BLOCKS = [
  EmberObject.create({
    blocks: [0, 5],
    provider: 'p1',
    getProvider: () => Promise.resolve({
      id: 'p1',
      name: 'Provider 1',
    }),
  }),
  EmberObject.create({
    blocks: [6, 10],
    provider: 'p2',
    getProvider: () => Promise.resolve({
      id: 'p2',
      name: 'Provider 2',
    }),
  }),
];

const StoreStub = Service.extend({
  query() {
    return Promise.resolve(FILE_BLOCKS);
  },
});

describe('Integration | Component | modals/file chunks', function() {
  setupComponentTest('modals/file-chunks', {
    integration: true
  });

  beforeEach(function () {
    registerService(this, 'store', StoreStub);
  });
  
  it('renders file blocks table with entries', function(done) {
    const closedSpy = sinon.spy();
    this.on('closedSpy', closedSpy);
    const file = EmberObject.create({
      id: 'some_id',
      size: 10,
    });
    this.set('file', file);
    
    this.render(hbs`{{modals/file-chunks
      open=true
      modalId="file-chunks"
      closedAction=(action "closedSpy")
      fileForChunks=file
      currentProviderSupport=false
      space=space
    }}`);
        
    setTimeout(() => {
      const $fileBlocksTable = this.$('.file-blocks-table');
      expect($fileBlocksTable.find('tr')).to.have.lengthOf(2);
      done();
    }, 300);
  });
});
