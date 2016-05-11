import Ember from 'ember';

export default Ember.Component.extend({
  secondaryMenu: Ember.inject.service(),

  classNames: ['service-debugger'],
});
