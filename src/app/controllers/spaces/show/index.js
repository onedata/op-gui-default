import Ember from 'ember';

export default Ember.Controller.extend({
  defaultOption: 'users',

  goToDefaultOption() {
    if (this.get('model')) {
      this.transitionToRoute(`spaces.show.${this.defaultOption}`, this.get('model'));
    }
  },

  /**
    Observe for a space to appear in model.
    When ready - load a default "users permissions".
  */
  onModelChange: function() {
    this.goToDefaultOption();
  }.observes('model')
});
