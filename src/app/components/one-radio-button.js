import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['one-option-button', 'col-xs-4'],

  checked: function() {
    return this.get('groupValue') === this.get('value');
  }.property('groupValue', 'value'),

  label: null,
  value: null,

  icon: function() {
    return this.get('checked') ? 'checkbox-option' : 'checkbox-empty';
  }.property('checked'),

  toggle() {
    this.set('groupValue', this.get('value'));
  },

  click() {
    this.toggle();
  }
});
