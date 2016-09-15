import Ember from 'ember';

export default Ember.Service.extend({
  controller: Ember.computed.oneWay('route.controller'),
});
