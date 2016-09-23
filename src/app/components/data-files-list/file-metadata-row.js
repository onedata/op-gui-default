import Ember from 'ember';

export default Ember.Component.extend({
  fileSystemTree: Ember.inject.service(),

  tagName: 'tr',
  classNames: ['first-level'],
  classNameBindings: ['highlightClass'],

  /**
   * To inject,
   * File which metadata is edited.
   * @type {File}
   */
  file: null,

  metadata: Ember.computed.alias('file.fileProperty.content'),

  isLoading: Ember.computed('metadata', function() {
    return !this.get('metadata');
  }),

  highlightClass: Ember.computed('file.isSelected', function() {
    return this.get('file.isSelected') ? 'active' : 'metadata-opened';
  }),

  actions: {
    closeMetadataEditor() {
      this.get('fileSystemTree').closeMetadataEditor(this.get('file'));
    }
  }
});
