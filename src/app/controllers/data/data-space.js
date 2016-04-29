import Ember from 'ember';

export default Ember.Controller.extend({
  fileSystemTree: Ember.inject.service(),

  onDataSpaceChange: function() {
    if (this.get('model')) {
      this.set('fileSystemTree.selectedSpace', this.get('model'));
    }
  }.observes('model')
});
