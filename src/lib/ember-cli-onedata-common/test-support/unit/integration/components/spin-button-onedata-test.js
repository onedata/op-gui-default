/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'spin-button-onedata',
  'Integration: SpinButtonOnedataComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#spin-button-onedata}}
      //     template content
      //   {{/spin-button-onedata}}
      // `);

      this.render(hbs`{{spin-button-onedata}}`);
      expect(this.$()).to.have.length(1);
    });

    // Testing some extensions of original spin-button

    it('allows to set size, which maps to data-size attribute', function () {
      this.render(hbs`
      {{#spin-button-onedata size="xs"}}
        hello
      {{/spin-button-onedata}}
      `);

      // TODO: integrate and use chai-jquery
      expect(this.$().find('button').attr('data-size')).to.be.equal('xs');
    });

    it('allows to set spinner size, which maps to data-spinner-size attribute', function () {
      this.render(hbs`
      {{#spin-button-onedata spinnerSize=99}}
        hello
      {{/spin-button-onedata}}
      `);

      // TODO: integrate and use chai-jquery
      expect(this.$().find('button').attr('data-spinner-size')).to.be.equal('99');
    });

    it('allows to set buttonType, which maps to button`s type attribute', function () {
      this.render(hbs`
      {{#spin-button-onedata buttonType='submit'}}
        hello
      {{/spin-button-onedata}}
      `);

      // TODO: integrate and use chai-jquery
      expect(this.$().find('button').attr('type')).to.be.equal('submit');
    });
  }
);
