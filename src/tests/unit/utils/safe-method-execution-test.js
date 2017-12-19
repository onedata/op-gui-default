import { expect } from 'chai';
import { describe, it } from 'mocha';
import safeMethodExecution from 'ember-cli-onedata-common/utils/safe-method-execution';
import wait from 'ember-test-helpers/wait';
import Ember from 'ember';

const {
  run,
} = Ember;

describe('Unit | Utility | safe method execution', function () {
  it('invokes method on valid object', function () {
    const testObject = Ember.Object.create({
      echo(p1, p2) {
        return [p1, p2];
      },
    });
    const result = safeMethodExecution(testObject, 'echo', 1, 2);

    expect(result).to.be.deep.equal([1, 2]);
  });

  it('does not throw exception if invoking method on destroyed object', function (done) {
    const testObject = Ember.Object.create({
      hello() {
        return 'world';
      },
    });

    run(() => testObject.destroy());

    wait().then(() => {
      const result = safeMethodExecution(testObject, 'hello');

      expect(result).to.be.undefined;
      done();
    });
  });

  it('allows to use function instead of method name', function () {
    const testObject = Ember.Object.create({
      something: 1,
    });
    const result = safeMethodExecution(testObject, function (x, y) {
      return this.get('something') + x + y;
    }, 10, 20);

    expect(result).to.be.equal(31);
  });
});
