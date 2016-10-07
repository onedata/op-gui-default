import Ember from 'ember';

const PER_LEVEL_PADDING_PX = 22;

export default Ember.Component.extend({
  fileBrowser: Ember.inject.service(),

  tagName: 'li',
  classNames: ['data-files-list-item'],

  /**
   * To inject.
   * @type {Number}
   */
  level: null,

  innerItemStyle: Ember.computed('level', function() {
    return Ember.String.htmlSafe(
      `padding-left: ${this.get('level')*PER_LEVEL_PADDING_PX}px;`
    );
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
