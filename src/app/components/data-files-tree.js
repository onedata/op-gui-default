import Ember from 'ember';

/**
 * Container for data-files-tree-nodes.
 *
 * Sends actions:
 * - openDirInBrowser(file) - open dir for browsing
 * @module components/data-files-tree
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  fileBrowser: Ember.inject.service(),
  fileSystemTreeService: Ember.inject.service('fileSystemTree'),

  classNames: ['data-files-tree'],

  /**
   * Reference to File - root of the filesystem showed in tree.
   * @type File
   */
  rootDir: null,

  didInsertElement() {
    const handleSelector = '#data-sidebar-resize-handler';
    const $resizeHandler = $(handleSelector);
    const $secondarySidebar = $('.secondary-sidebar');
    // resize handler is positioned absolutely to main-content
    $resizeHandler.css('left', $secondarySidebar.width() - $resizeHandler.width()/2);
    $('.secondary-sidebar').resizable({
      handleSelector: handleSelector,
      resizeHeight: false,
      onDrag(e, $el, newWidth/*, newHeight, opt*/) {
        $resizeHandler.css('left', newWidth - $resizeHandler.width()/2);
      }
    });
  },

  actions: {
    /** Typically invoked by actions passed up from tree nodes */
    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    }
  }
});
