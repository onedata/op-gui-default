/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'modals/message-box',
  'Integration: ModalsMessageBoxComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#modals/message-box}}
      //     template content
      //   {{/modals/message-box}}
      // `);

      this.render(hbs`{{modals/message-box}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
