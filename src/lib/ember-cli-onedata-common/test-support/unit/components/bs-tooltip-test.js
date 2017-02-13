/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import sinon from 'sinon';
import sinonChai from 'ember-cli-onedata-common/exports/sinon-chai';
import chai from 'chai';
chai.use(sinonChai);

describe('BsTooltipComponent', function() {
  setupComponentTest('bs-tooltip', {
    // Specify the other units that are required for this test
    // needs: ['component:foo', 'helper:bar'],
    unit: true
  });

  it('renders', function() {
    // creates the component instance
    let component = this.subject();
    // renders the component on the page
    this.render();
    expect(component).to.be.ok;
    expect(this.$()).to.have.length(1);
  });

  it('should render only provider inner HTML as content', function () {
    let innerContent = '<strong>Hello <i>world</i></strong>';

    this.subject({
      innerContent: innerContent,
      layout: hbs`
        {{#bs-tooltip}}{{innerContent}}{{/bs-tooltip}}
      `
    });

    expect(this.$().text().trim()).to.be.equal(innerContent);
  });

  it('should invoke bootstrapize method when title is changed', function() {
    // given
    let bootstrapizeFun = sinon.stub(this.subject(), 'bootstrapize');
    try {
      // when
      this.subject().set('title', 'hello');

      // then
      expect(bootstrapizeFun).to.have.been.called;
    } finally {
      bootstrapizeFun.restore();
    }
  });
});
