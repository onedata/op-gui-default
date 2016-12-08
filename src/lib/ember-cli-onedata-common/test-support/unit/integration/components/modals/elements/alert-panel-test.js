/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'modals/elements/alert-panel',
  'Integration: ModalsElementsAlertPanelComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#modals/elements/alert-panel}}
      //     template content
      //   {{/modals/elements/alert-panel}}
      // `);

      this.render(hbs`{{modals/elements/alert-panel}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
