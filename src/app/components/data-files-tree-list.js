import Ember from 'ember';

/**
 * A node in a files tree. Should be a directory.
 *
 * Sends actions:
 * - openDirInBrowser(file) - should open a dir in a browser (data-files-list)
 * @module components/data-files-tree-list
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  fileBrowser: Ember.inject.service(),

  tagName: 'ul',

  classNames: ['data-files-tree-list'],

  /**
   * To inject - a dir which this node represents
   */
  rootDir: null,

  /**
   * Level of subdirectory in tree.
   * Currently only 6 levels are diplayed correctly!
   * @type {Number}
   */

  subdirsSorting: ['name:asc'],
  subdirs: Ember.computed.filterBy('rootDir.children', 'isDir', true),
  visibleSubdirs: Ember.computed.filter('subdirs', (sd) => sd.get('id') && sd.get('name')),
  visibleSubdirsSorted: Ember.computed.sort('visibleSubdirs', 'subdirsSorting'),

  actions: {
    /** Pass the action up (action goes up from child dirs) */
    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    }
  }
});
