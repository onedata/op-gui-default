import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';
import Ember from 'ember';
import wait from 'ember-test-helpers/wait';

const {
  RSVP: {
    Promise,
  },
} = Ember;

describe('Integration | Component | transfers/transfer chart', function() {
  setupComponentTest('transfers/transfer-chart', {
    integration: true
  });

  beforeEach(function () {
    this.set('transfer', Ember.Object.create({
      startTime: 1522745355,
      isCurrent: true,
      minuteStat: PromiseObject.create({
        promise: Promise.resolve({
          timestamp: 1522745361,
          type: 'minute',
          stats: {
            'someproviderid': [],
          },
        }),
      }),
      currentStat: PromiseObject.create({
        promise: Promise.resolve({}),
      }),
      status: 'replicating',
      on(event, f) {
        if (event === 'didLoad') {
          f();
        }
      },
    }));
  });

  it('shows loader if transfer stats are not loaded', function (done) {
    this.set('transfer.minuteStat', PromiseObject.create({ promise: new Promise(() => {}) }));
    this.render(hbs`{{transfers/transfer-chart transfer=transfer _updaterEnabled=false}}`);
    wait().then(() => {
      expect(this.$('.spin-spinner')).to.exist;
      done();
    });
  });

  it('shows loader if transfer status is not loaded', function (done) {
    this.set('transfer.currentStat', PromiseObject.create({ promise: new Promise(() => {}) }));
    this.render(hbs`{{transfers/transfer-chart transfer=transfer _updaterEnabled=false}}`);
    wait().then(() => {
      expect(this.$('.spin-spinner')).to.exist;
      done();
    });
  });

  it('shows info, that transfer is scheduled and stats are not available', function (done) {
    this.set('transfer.status', 'scheduled');
    this.render(hbs`{{transfers/transfer-chart transfer=transfer _updaterEnabled=false}}`);
    wait().then(() => {
      expect(this.$('.transfer-scheduled-info')).to.exist;
      done();
    });
  });

  it('shows info, that backend is while gathering statistics', function (done) {
    this.render(hbs`{{transfers/transfer-chart transfer=transfer _updaterEnabled=false}}`);
    wait().then(() => {
      expect(this.$('.stats-delay-info')).to.exist;
      done();
    });
  });

  it('shows chart if statistics were gathered and all data is loaded', function (done) {
    this.set('transfer.startTime', this.get('transfer.startTime') - 40);
    this.render(hbs`{{transfers/transfer-chart transfer=transfer _updaterEnabled=false}}`);
    wait().then(() => {
      expect(this.$('.ct-chart')).to.exist;
      done();
    });
  });
});
