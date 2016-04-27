import Ember from 'ember';

export default Ember.Component.extend({
  commonLoader: Ember.inject.service(),

  classNames: ['common-loader'],
  classNameBindings: ['isLoading::hidden'],

  isLoading: Ember.computed.alias('commonLoader.isLoading'),
  message: Ember.computed.alias('commonLoader.message'),
  messageSecondary: Ember.computed.alias('commonLoader.messageSecondary'),

  registerInService: function() {
    this.set('commonLoader.component', this);
  }.on('init'),
});
