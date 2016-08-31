import Ember from 'ember';

export default Ember.Route.extend({
  mainRouteName: 'shares',

  model() {
    return this.store.findAll('share');
  },

  actions: {
    /** Show Share */
    goToShare(share) {
      this.transitionTo('shares.show', share);
    },
  }
});
