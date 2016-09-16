import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',
  classNameBindings: [
    'file.isSelected:active',
    'isDownloading:selection-background-pulse'
  ],

  /**
   * To inject - a file that the row represents
   * @type File
   */
  file: null,

  isDownloading: false,

  isShowingMetadata: false,

  // TODO: make a component for file/dir icon

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
    shareFile() {
      this.sendAction('openFileShareModal', this.get('file'));
    },
    showFileMetadata() {
      // this.set('isShowingMetadata', true);
      this.sendAction('showFileMetadata', this.get('file'));
    },
    hideFileMetadata() {
      // this.set('isShowingMetadata', false);
      this.sendAction('hideFileMetadata', this.get('file'));
    }
  }
});
