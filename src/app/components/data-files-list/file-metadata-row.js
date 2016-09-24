import Ember from 'ember';

export default Ember.Component.extend({
  notify: Ember.inject.service(),
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

  /**
   * Handle remove result of metadata
   * @param {Boolean} [failed] if true, meta record destroy failed (it is not destroyed)
   * @param {Object} [error] error object, that should be present only of ``failed`` is true
   */
  handleMetadataRemoved(failed, error) {
    const fileName = this.get('file.name');
    // FIXME: translate
    if (failed === true) {
      const errorMsg = error.message ? (": " + error.message) : "";
      this.get('notify').error(`Could not delete metadata for file "${fileName}${errorMsg}"`);
    } else {
      this.get('notify').info(`Deleted metadata for file "${fileName}"`);
      this.send('closeMetadataEditor');
    }
  },

  actions: {
    closeMetadataEditor() {
      this.get('fileSystemTree').closeMetadataEditor(this.get('file'));
    },

    /**
     * Remove metadata record and close metadata editor.
     * This should be invoked only if metadata is already loaded.
     */
    removeMetadata() {
      const metadata = this.get('metadata');
      if (metadata.get('isNew')) {
        metadata.rollbackAttributes();
        this.handleMetadataRemoved();
      } else {
        const destroyPromise = metadata.destroyRecord();

        destroyPromise.then(() => {
          this.handleMetadataRemoved();
        });

        destroyPromise.catch((error) => {
          this.handleMetadataRemoved(true, error);
        });
      }
    }
  }
});
