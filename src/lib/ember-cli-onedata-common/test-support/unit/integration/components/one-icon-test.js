/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'one-icon',
  'Integration: OneIconComponent',
  {
    integration: true
  },
  function() {
    it('renders element with oneicon-<name> provided with icon property', function() {
      this.render(hbs`{{one-icon icon="space"}}`);
      let $oneicon = this.$().find('.one-icon');
      expect($oneicon).to.have.class('oneicon-space');
    });
    it('propagates passed classes and preserves icon class', function() {
      this.render(hbs`{{one-icon icon="space" class="some-class other-class"}}`);
      let $oneicon = this.$().find('.one-icon');
      expect($oneicon).to.have.class('oneicon-space');
      expect($oneicon).to.have.class('some-class');
      expect($oneicon).to.have.class('other-class');
    });
  }
);
