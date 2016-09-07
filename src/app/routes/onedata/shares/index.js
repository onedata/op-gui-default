import Ember from 'ember';

// FIXME: jsdoc
export default Ember.Route.extend({
  model() {
    return this.modelFor('onedata.shares');
  },

  setupController(controller, model) {
    this._super(controller, model);
    controller.goToDefault();
  },
});
