import { expect } from 'chai';
import { describe, it } from 'mocha';
import Ember from 'ember';
import Looper from 'onedata-gui-common/utils/looper';

const {
  run
} = Ember;

describe('Unit | Utility | looper', function () {
  it('invokes registered events in given interval', function (done) {
    let counter = 0;
    let timer = Looper.create({
      immediate: false,
      interval: 50,
    });

    timer.on('tick', function () {
      counter += 1;
    });

    setTimeout(function () {
      try {
        expect(counter).to.be.gte(2).and.lte(3);
      } finally {
        run(() => timer.destroy());
      }
      done();
    }, 150);
  });

  it('can stop interval', function (done) {
    let counter = 0;
    let timer = Looper.create({
      immediate: false,
      interval: 50,
    });

    timer.stop();
    timer.on('tick', function () {
      counter += 1;
    });

    setTimeout(function () {
      try {
        expect(counter).to.be.equal(0);
      } finally {
        run(() => timer.destroy());
      }
      done();
    }, 150);
  });
});
