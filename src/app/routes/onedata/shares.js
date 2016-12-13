import Ember from 'ember';

export default Ember.Route.extend({
  mainRouteName: 'shares',

  beforeModel() {
    // TODO: this causes full reload of shared files, try to use unloadAll
    this.store.peekAll('file-shared').forEach(r => r.reload());
  },

  model() {
    return this.store.findAll('share');
  },

  actions: {
    /** Show Share */
    goToShare(share) {
      this.transitionTo('onedata.shares.show', share);
    },
  }
});
