/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'share-info-head',
  'Integration: ShareInfoHeadComponent',
  {
    integration: true
  },
  function() {
    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#share-info-head}}
      //     template content
      //   {{/share-info-head}}
      // `);

      this.render(hbs`{{share-info-head}}`);
      expect(this.$()).to.have.length(1);
    });

    it('displays the share name', function() {
      const share = {
        name: 'Hello share'
      };

      this.set('share', share);

      this.render(hbs`{{share-info-head share=share}}`);
      expect(this.$()).to.contain(share.name);
    });
  }
);
