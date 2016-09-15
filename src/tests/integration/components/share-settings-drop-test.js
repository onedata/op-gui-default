/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'share-settings-drop',
  'Integration: ShareSettingsDropComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#share-settings-drop}}
      //     template content
      //   {{/share-settings-drop}}
      // `);

      this.render(hbs`{{share-settings-drop}}`);
      expect(this.$()).to.have.length(1);
    });
  }
);
