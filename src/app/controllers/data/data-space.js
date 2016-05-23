import Ember from 'ember';

/**
 * Main purpose is to watch a model (selected space) for change.
 * @module controllers/data/data-space
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  fileSystemTree: Ember.inject.service(),
  i18n: Ember.inject.service(),

  pageTitle: function() {
    return `${this.get('i18n').t('data.dataSpace.title')} "${this.get('model.name')}"`;
  }.property('model.name'),

  onDataSpaceChange: function() {
    if (this.get('model')) {
      this.set('fileSystemTree.selectedSpace', this.get('model'));
    }
  }.observes('model')
});
