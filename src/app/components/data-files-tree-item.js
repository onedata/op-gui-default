import Ember from 'ember';

export default Ember.Component.extend({
  fileBrowser: Ember.inject.service(),

  tagName: 'li',
  classNames: ['data-files-list-item'],
  classNameBindings: ['levelClass'],

  /**
   * To inject.
   * @type {Number}
   */
  level: null,

  levelClass: Ember.computed('level', function() {
    let level = this.get('level');
    return level ? `level-${level}` : '';
  }),

  actions: {
    toggleDir() {
      this.set('file.isExpanded', !this.get('file.isExpanded'));
    },

    browseDir() {
      if (this.get('file.isDir')) {
        this.sendAction('openDirInBrowser', this.get('file'));
      } else {
        console.error(`Tried to browse a file in file brower (should be dir): ${this.get('file.id')}`);
      }
    },

    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    },
  }
});
