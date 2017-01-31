import Ember from 'ember';

export default Ember.Service.extend({
  component: null,
  // TODO: to extend - there can be many common-loader clients, so some can load and some not
  isLoading: false,
  message: null,
  messageSecondary: null,
});
