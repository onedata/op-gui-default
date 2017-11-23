import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import Ember from 'ember';
import wait from 'ember-test-helpers/wait';

const {
  Object: EmberObject,
  A,
} = Ember;

describe('Integration | Component | transfers/data container', function () {
  setupComponentTest('transfers/data-container', {
    integration: true
  });

  beforeEach(function () {
    const bps1 = {
      p2: 1,
      p3: 3,
    };
    const bps2 = {
      p3: 7,
    };
    const transfers = A([
      EmberObject.create({
        destination: 'p1',
        bytesPerSec: bps1,
        tableDataIsLoaded: true,
      }),
      EmberObject.create({
        destination: 'p2',
        bytesPerSec: bps2,
        tableDataIsLoaded: true,
      }),
    ]);
    transfers.isLoaded = true;
    const space = EmberObject.create({
      currentTransferList: {
        list: {
          content: transfers,
        },
      },
    });
    this.set('space', space);
  });

  it('yields destinationProviderIds', function (done) {
    let yieldedValue;
    const checkYield = function () {
      yieldedValue = this.get('ids');
    };
    this.set('checkYield', checkYield);

    this.render(hbs `
      {{#transfers/data-container
        isSupportedByCurrentProvider=true
        transfersUpdaterEnabled=false
        space=space
        as |tData|
      }}
        {{test-callback
          callback=checkYield
          ids=tData.destinationProviderIds
        }}
      {{/transfers/data-container}}
    `);

    wait().then(() => {
      expect(yieldedValue).to.have.lengthOf(2);
      expect(yieldedValue).to.include('p1');
      expect(yieldedValue).to.include('p2');
      done();
    });
  });

  it('yields sourceProviderIds', function (done) {
    let yieldedValue;
    const checkYield = function () {
      yieldedValue = this.get('ids');
    };
    this.set('checkYield', checkYield);

    this.render(hbs `
      {{#transfers/data-container
        isSupportedByCurrentProvider=true
        transfersUpdaterEnabled=false
        space=space
        as |tData|
      }}
        {{test-callback
          callback=checkYield
          ids=tData.sourceProviderIds
        }}
      {{/transfers/data-container}}
    `);

    wait().then(() => {
      expect(yieldedValue).to.have.lengthOf(2);
      expect(yieldedValue).to.include('p2');
      expect(yieldedValue).to.include('p3');
      done();
    });
  });
  
  it('yields computed providerTransfers', function (done) {    
    let providerTransfers;
    const checkYield = function () {
      providerTransfers = this.get('providerTransfers');
    };
    this.set('checkYield', checkYield);

    this.render(hbs `
      {{#transfers/data-container
        isSupportedByCurrentProvider=true
        transfersUpdaterEnabled=false
        space=space
        as |tData|
      }}
        {{test-callback
          callback=checkYield
          providerTransfers=tData.providerTransfers
        }}
      {{/transfers/data-container}}
    `);
    
    wait().then(() => {
      // p2->p1, p3->p1, p3->p2
      expect(providerTransfers).to.have.lengthOf(3);
      // more tests for providerTransfers computation can be found
      // in tests for util:providerTransfers
      done();
    });
  });
});
