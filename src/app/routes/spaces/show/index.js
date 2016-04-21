import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    return this.modelFor('spaces.show');
  },

  activate() {
    this.controllerFor(this.routeName).goToDefaultOption();
  },
});
