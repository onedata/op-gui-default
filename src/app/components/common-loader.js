import Ember from 'ember';


/**
 * FIXME: doc
 * @module components/common-loader
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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
