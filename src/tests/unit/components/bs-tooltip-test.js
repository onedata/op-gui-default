/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'bs-tooltip',
  'BsTooltipComponent',
  {
    // Specify the other units that are required for this test
    // needs: ['component:foo', 'helper:bar'],
    unit: true
  },
  function() {
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
  }
);
