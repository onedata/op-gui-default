import Ember from 'ember';

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
