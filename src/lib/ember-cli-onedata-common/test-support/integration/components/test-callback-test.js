import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import sinon from 'sinon';

describe('Integration | Component | test callback', function() {
  setupComponentTest('test-callback', {
    integration: true
  });

  it('immediately invokes callback', function() {
    const spy = sinon.spy();
    const callback = function () {
      spy(this.get('x'), this.get('y'));
    };
    this.set('callback', callback);
    this.render(hbs`
      {{test-callback callback=callback x=1 y=2}}
    `);
    expect(spy).to.be.calledOnce;
    expect(spy).to.be.calledWith(1, 2);
  });
});
