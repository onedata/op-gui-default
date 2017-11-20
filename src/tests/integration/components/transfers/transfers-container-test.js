import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import Ember from 'ember';
// import mockHasMany from 'ember-cli-onedata-common/utils/mock-has-many';
import wait from 'ember-test-helpers/wait';

const {
  Object: EmberObject,
  A,
} = Ember;

describe('Integration | Component | transfers/transfers container', function () {
  setupComponentTest('transfers/transfers-container', {
    integration: true
  });

  beforeEach(function () {
    const bps1 = {
      p2: [],
      p3: [],
    };
    const bps2 = {
      p2: [],
      p3: [],
    };
    const transfers = A([
      EmberObject.create({
        destination: 'p1',
        bytesPerSec: bps1,
      }),
      EmberObject.create({
        destination: 'p2',
        bytesPerSec: bps2,
      }),
    ]);
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
      {{#transfers/transfers-container
        isSupportedByCurrentProvider=true
        transfersUpdaterEnabled=false
        space=space
        as |tData|
      }}
        {{test-callback
          callback=checkYield
          ids=tData.destinationProviderIds
        }}
      {{/transfers/transfers-container}}
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
      {{#transfers/transfers-container
        isSupportedByCurrentProvider=true
        transfersUpdaterEnabled=false
        space=space
        as |tData|
      }}
        {{test-callback
          callback=checkYield
          ids=tData.sourceProviderIds
        }}
      {{/transfers/transfers-container}}
    `);

    wait().then(() => {
      expect(yieldedValue).to.have.lengthOf(2);
      expect(yieldedValue).to.include('p2');
      expect(yieldedValue).to.include('p3');
      done();
    });
  });
});
