import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | bezier curve', function () {
  setupComponentTest('bezier-curve', {
    integration: true
  });

  it('renders', function () {
    this.render(hbs `{{bezier-curve x1=100 y1=100 x2=200 y2=100 curveFactor=1}}`);
    expect(this.$('path').attr('d')).to.be.equal('M 100 100 Q 150 50, 200 100');
  });
});
