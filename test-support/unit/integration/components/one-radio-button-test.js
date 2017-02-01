/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

const CHECKED_CLASS = 'checked';
const NOT_CHECKED_CLASS = 'not-checked';

const CHECKED_ICON = 'checkbox-option';
const NOT_CHECKED_ICON = 'checkbox-empty';

describe('Integration: OneRadioButtonComponent', function() {
  setupComponentTest('one-radio-button', {
    integration: true
  });

  it('has checked class and icon when value is the same as injected groupValue', function() {
    this.render(hbs`
    {{one-radio-button groupValue="one" value="one"}}
    `);
    let $radioButton = this.$('.one-option-button');

    expect($radioButton).to.have.class(CHECKED_CLASS);
    expect($radioButton).to.not.have.class(NOT_CHECKED_CLASS);
    expect($radioButton.find('.oneicon-' + CHECKED_ICON).length).to.equal(1);
  });
  it('has not-checked class and icon when value is not the same as injected groupValue', function() {
    this.render(hbs`
    {{one-radio-button groupValue="one" value="two"}}
    `);
    let $radioButton = this.$('.one-option-button');

    expect($radioButton).to.have.class(NOT_CHECKED_CLASS);
    expect($radioButton).to.not.have.class(CHECKED_CLASS);
    expect($radioButton.find('.oneicon-' + NOT_CHECKED_ICON).length).to.equal(1);
  });
  it('changes groupValue to its value then clicked', function() {
    this.set('groupValue', 'one');
    this.render(hbs`
    {{one-radio-button groupValue=groupValue value="two"}}
    `);
    let $radioButton = this.$('.one-option-button');

    $radioButton.click();

    expect(this.get('groupValue')).to.equal('two');
  });
});
