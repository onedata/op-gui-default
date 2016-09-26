import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',
  classNames: ['first-level'],
  classNameBindings: [
    'highlightClass',
    'isDownloading:selection-background-pulse'
  ],

  highlightClass: Ember.computed('file.isSelected', 'file.isEditingMetadata', function() {
    return this.get('file.isSelected') && 'active' ||
      this.get('file.isEditingMetadata') && 'metadata-opened' ||
      '';
  }),

  /**
   * To inject - a file that the row represents
   * @type File
   */
  file: null,

  /**
   * If true, the file is currently downloaded, so it will be indicated in GUI.
   * @type {Boolean}
   */
  isDownloading: false,

  /**
   * To inject.
   * If true, tools for file manipulation are disabled.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  click() {
    this.sendAction('selectFile', this.get('file'));
  },

  doubleClick() {
    if (this.get('file.isDir')) {
      this.sendAction('openDirInBrowser', this.get('file'));
    } else {
      this.set('isDownloading', true);
      const p = new Ember.RSVP.Promise((resolve, reject) => {
        this.sendAction('downloadFile', this.get('file'), resolve, reject);
      });
      p.finally(() => this.set('isDownloading', false));
    }
  },

  actions: {
    shareFile(file) {
      this.sendAction('openFileShareModal', file || this.get('file'));
    },
    toggleFileMetadata(file) {
      this.sendAction('toggleFileMetadata', file || this.get('file'));
    }
  }
});
