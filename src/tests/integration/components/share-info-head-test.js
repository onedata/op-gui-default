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
    it('displays the share name', function() {
      const share = {
        name: 'Hello share'
      };

      this.set('share', share);

      this.render(hbs`{{share-info-head share=share}}`);
      expect(this.$()).to.contain(share.name);
    });

    it('displays a public url of share in input element', function() {
      const share = {
        name: 'Share with link',
        publicUrl: 'https://example.com'
      };

      this.set('share', share);

      this.render(hbs`{{share-info-head share=share}}`);
      expect(this.$().find('input')[0].value).to.equal(share.publicUrl);
    });
  }
);
