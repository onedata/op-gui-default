import Ember from 'ember';

export default Ember.Component.extend({
  fileSystemTree: Ember.inject.service(),

  classNames: ['file-row-tools'],
  classNameBindings: ['highlightClass'],

  /**
   * To inject.
   * A file that this tool operate on.
   * @type {File}
   */
  file: null,

  /**
   * To inject.
   * If true, tools for file manipulation are disabled.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  metadataClass: Ember.computed('file.isEditingMetadata', 'file.hasMetadata', function() {
    if (this.get('file.isEditingMetadata')) {
      return 'active';
    } else if (this.get('file.hasMetadata')) {
      return 'visible-on-parent-hover-25p';
    } else if (this.get('readOnly')) {
      return 'hidden';
    } else {
      return 'visible-on-parent-hover';
    }
  }),

  // TODO: use tooltips!
  metadataIconTip: Ember.computed('file.hasMetadata', function() {
    // TODO: translate
    return this.get('file.hasMetadata') ? 'Toggle metadata editor' : 'Initialize metadata';
  }),

  actions: {
    shareFile() {
      this.sendAction('shareFile', this.get('file'));
    },
    metadataClicked() {
      if (this.get('file.hasMetadata')) {
        this.send('toggleFileMetadata');
      } else if (!this.get('readOnly')) {
        this.send('createFileMetadata');
      }
    },
    toggleFileMetadata() {
      this.sendAction('toggleFileMetadata', this.get('file'));
    },
    createFileMetadata() {
      const file = this.get('file');
      this.get('fileSystemTree').toggleMetadataEditor(file);
    }
  }
});
