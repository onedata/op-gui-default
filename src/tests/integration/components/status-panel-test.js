/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'status-panel',
  'Integration: StatusPanelComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#status-panel}}
      //     template content
      //   {{/status-panel}}
      // `);

      this.render(hbs`{{status-panel}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
