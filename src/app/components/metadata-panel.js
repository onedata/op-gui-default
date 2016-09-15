import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['metadata-panel'],

  init() {
    this._super(...arguments);
  },

  didInsertElement() {
    this.$().find('ul').addClass('nav-tabs');
  },
});
