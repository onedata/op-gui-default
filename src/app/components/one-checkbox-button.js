import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['one-option-button', 'col-xs-4'],

  label: null,
  value: null,

  init() {
    this._super();

    const prop = `groupValues.${this.get('value')}`;

    this.checked = Ember.computed(prop, {
      get() {
        return this.get(prop);
      },
      set(key, value) {
        this.set(prop, value);
        return value;
      }
    });
  },

  icon: function() {
    return this.get('checked') ? 'checkbox-filled' : 'checkbox-empty';
  }.property('checked'),

  toggle() {
    this.set('checked', !this.get('checked'));
  },

  click() {
    this.toggle();
  }
});
