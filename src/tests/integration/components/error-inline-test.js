import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import { registerService } from '../../helpers/stub-service';
import Ember from 'ember';

const {
  Service,
} = Ember;

describe('Integration | Component | error inline', function() {
  setupComponentTest('error-inline', {
    integration: true
  });

  const i18n = Service.extend({
    t(id) {
      switch (id) {
        case 'components.errorInline.defaultShort':
          return '(error)'; 
        default:
          return `en: <${id}>`;
      }
    }
  });
  
  beforeEach(function () {
    registerService(this, 'i18n', i18n);
  });
  
  it('renders custom short error text', function() {
    this.render(hbs`
      {{#error-inline}}
        short error text
      {{/error-inline}}
    `);
    
    expect(this.$('.error-inline').text()).to.match(/short error text/);
  });
  
  it('renders default short error text', function() {
    this.render(hbs`
      {{error-inline}}
    `);
    
    const text = this.$('.error-inline').text();
    expect(text).to.not.match(/components\./);
    expect(text).to.match(/error/);
  });
  
  it('is an inline element', function () {
    this.render(hbs`
      {{error-inline}}
    `);
    
    const style = window.getComputedStyle(this.$('.error-inline')[0]);
    const display = style.getPropertyValue('display');
    expect(display).to.equal('inline');
  });
  
  it('has other color than adjacent element default color', function () {
    this.render(hbs`
      <span id="one">hello</span>
      {{error-inline}}
    `);
    
    const defaultStyle = window.getComputedStyle(this.$('#one')[0]);
    const errorStyle = window.getComputedStyle(this.$('.error-inline')[0]);
    const defaultColor = defaultStyle.getPropertyValue('color');
    const errorColor = errorStyle.getPropertyValue('color');
    expect(errorColor).to.be.not.equal(defaultColor);
  });
});
