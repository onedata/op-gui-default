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
  }
);
