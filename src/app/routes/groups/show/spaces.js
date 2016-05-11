import Ember from 'ember';

// TODO: doc
export default Ember.Route.extend({
  model() {
    return this.modelFor('groups.show');
  },

  actions: {
    didTransition() {
      this.controllerFor(this.routeName).changeMenuActiveOption();
    },
  }
});
