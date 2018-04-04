import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import Ember from 'ember';
import wait from 'ember-test-helpers/wait';

const {
  Object: EmberObject,
  ObjectProxy,
  ArrayProxy,
  A,
} = Ember;

describe('Integration | Component | transfers/data container', function () {
  setupComponentTest('transfers/data-container', {
    integration: true
  });

  beforeEach(function () {
    const transfers = A([
      EmberObject.create({
        destination: 'p1',
        tableDataIsLoaded: true,
      }),
      EmberObject.create({
        destination: 'p2',
        tableDataIsLoaded: true,
      }),
    ]);
    const listRelation = {
      ids() {
        return ['p1', 'p2'];
      }
    };
    transfers.isLoaded = true;
    const space = EmberObject.create({
      currentTransferList: ObjectProxy.create({
        content: {
          list: ArrayProxy.create({
            content: transfers,
          }),
          hasMany() {
            return listRelation;
          },
        },
      }),
      transferProviderMap: ObjectProxy.create({
        content: Ember.Object.create({
          mapping: {
            p2: ['p1'],
            p3: ['p1', 'p2'],
          },
        }),
      }),
    });
    this.set('space', space);
    
    const transfersErrored = A([
      EmberObject.create({
        destination: 'p1',
        tableDataIsLoaded: true,
        currentStatError: true,
      }),
      EmberObject.create({
        destination: 'p2',
        tableDataIsLoaded: true,
        currentStatError: false,
      }),
    ]);
    
    const spaceErrored = EmberObject.create({
      currentTransferList: ObjectProxy.create({
        content: {
          list: ArrayProxy.create({
            content: transfersErrored,
          }),
          hasMany() {
            return listRelation;
          },
        },
      }),
    });
    this.set('spaceErrored', spaceErrored);
  });

  it('yields destinationProviderIds', function (done) {
    let yieldedValue;
    const checkYield = function (testCallbackComponent) {
      yieldedValue = testCallbackComponent.get('ids');
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
    const checkYield = function (testCallbackComponent) {
      yieldedValue = testCallbackComponent.get('ids');
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
  
  it('yields computed providerTransferConnections', function (done) {    
    let providerTransferConnections;
    const checkYield = function (tc) {
      providerTransferConnections = tc.get('providerTransferConnections');
    };
    this.set('checkYield', checkYield);

    this.render(hbs `
      {{#transfers/data-container
        currentTransfersLoaded=true
        isSupportedByCurrentProvider=true
        transfersUpdaterEnabled=false
        space=space
        as |tData|
      }}
        {{test-callback
          callback=checkYield
          providerTransferConnections=tData.providerTransferConnections
        }}
      {{/transfers/data-container}}
    `);
    
    wait().then(() => {
      // p2->p1, p3->p1, p3->p2
      expect(providerTransferConnections).to.have.lengthOf(3);
      done();
    });
  });  
});
