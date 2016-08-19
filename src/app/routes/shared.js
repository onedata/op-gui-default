import Ember from 'ember';

export default Ember.Route.extend({
  mainRouteName: 'shared',

  model() {
    return this.store.findAll('share');
  },

  actions: {
    /** Show Share */
    goToShare(share) {
      this.transitionTo('shared.show', share);
    },
  }
});
