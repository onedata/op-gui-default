import Ember from 'ember';


/**
 * A global (one instance for application) component for common-loader service.
 * It allows to show loading information with spinner and two lines of text.
 * @module components/common-loader
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  commonLoader: Ember.inject.service(),

  classNames: ['common-loader', 'semi-transparent'],
  classNameBindings: ['isLoading::hidden', 'areaClass'],

  areaClass: Ember.computed('commonLoader.area', function() {
    const _area = this.get('commonLoader.area');
    return `loader-area-${_area}`;
  }),

  area: Ember.computed.alias('commonLoader.area'),
  type: Ember.computed.alias('commonLoader.type'),
  isLoading: Ember.computed.alias('commonLoader.isLoading'),
  message: Ember.computed.alias('commonLoader.message'),
  messageSecondary: Ember.computed.alias('commonLoader.messageSecondary'),

  // if we turn off loader, reset all properties
  isLoadingChanged: Ember.observer('isLoading', function() {
    if (!this.get('isLoading')) {
      this.resetProperties();
    }
  }),

  resetProperties() {
    this.setProperties({
      message: null,
      messageSecondary: null,
      area: null,
      type: null,
    });
  },

  registerInService: function() {
    this.set('commonLoader.component', this);
  }.on('init'),
});
