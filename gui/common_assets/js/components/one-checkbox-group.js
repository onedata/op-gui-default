import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['one-option-group'],

  init() {
    this._super();

    // this property will contain a dictionary of checkbox options
    if (this.get('groupValues') == null) {
      this.set('groupValues', Ember.Object.create());
    }
  }
});
