import Ember from 'ember';

export default Ember.Controller.extend({
  fileSystemTree: Ember.inject.service(),

  /**
   * If true, show share-info modal
   * @type Boolean
   */
  isShowingShareInfo: false,
  isCreatingShare: false,

  fileShareFile: null,

  /**
   * Will be injected by data-space controller
   */
  dataSpace: Ember.computed.alias('fileSystemTree.selectedSpace'),
});
