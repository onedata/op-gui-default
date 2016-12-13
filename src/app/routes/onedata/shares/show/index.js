import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    return this.modelFor('onedata.shares.show');
  },

  redirect(shareModel) {
    this.transitionTo('onedata.shares.show.dir', shareModel.get('containerDir'));
  }
});
