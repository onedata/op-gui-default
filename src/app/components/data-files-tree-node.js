import Ember from 'ember';

/**
 * A node in a files tree. Should be a directory.
 *
 * Sends actions:
 * - openDirInBrowser(fileId) - should open a dir in a browser (data-files-list)
 * @module components/data-files-tree-node
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  /**
    Level of subdirectory in tree.
    Currently only 6 levels are diplayed correctly!
  */
  level: null,

  nextLevel: function() {
    return this.get('level') + 1;
  }.property('level'),

  /** CSS class for node elements to apply proper padding */
  levelClass: function() {
    let level = this.get('level');
    return level ? `level-${level}` : '';
  }.property('level'),

  subdirsSorting: ['name:asc'],
  subdirs: Ember.computed.filterBy('rootDir.children', 'isDir', true),
  visibleSubdirs: Ember.computed.filter('subdirs', (sd) => sd.get('id') && sd.get('name')),
  visibleSubdirsSorted: Ember.computed.sort('visibleSubdirs', 'subdirsSorting'),

  actions: {
    /** Expand/collapse a dir, showing/hiding its children */
    toggleDir(dirFile) {
      dirFile.set('isExpanded', !dirFile.get('isExpanded'));
    },

    browseDir(dirFile) {
      if (dirFile.get('isDir')) {
        this.sendAction('openDirInBrowser', dirFile.get('id'));
      } else {
        console.error(`Tried to browse a file in file brower (should be dir): ${dirFile.get('id')}`);
      }
    },

    /** Pass the action up (action goes up from child dirs) */
    openDirInBrowser(fileId) {
      this.sendAction('openDirInBrowser', fileId);
    }
  }
});
