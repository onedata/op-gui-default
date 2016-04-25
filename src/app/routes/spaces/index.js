import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    return this.modelFor('spaces');
  },

  activate() {
    this.controllerFor(this.routeName).goToDefaultSpace();
  },
});
