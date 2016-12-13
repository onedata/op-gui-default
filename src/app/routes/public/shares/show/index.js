import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    return this.modelFor('public.shares.show');
  },

  redirect(shareModel) {
    this.transitionTo('public.shares.show.dir', shareModel.get('containerDir'));
  }
});
