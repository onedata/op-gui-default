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
  }
});
