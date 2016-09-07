import Ember from 'ember';

// FIXME: jsdoc
export default Ember.Controller.extend({
  goToDefault() {
    console.debug(`shares.index: Will try to go to first share`);
    let shares = this.get('model');
    if (shares) {
      this.transitionToRoute('onedata.shares.show', shares.sortBy('name').get('firstObject'));
    }
  },

  onModelChange: Ember.observer('model.[]', 'model.@each.id', function() {
    this.goToDefault();
  }),
});
