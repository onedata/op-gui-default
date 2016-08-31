import Ember from 'ember';

export default Ember.Route.extend({
  model(params) {
    return this.handleReject(this.store.find('share', params.share_id));
  },
});
