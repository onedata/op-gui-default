import Ember from 'ember';

/**
 * Container for data-files-tree-list(s).
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
  eventsBus: Ember.inject.service(),

  classNames: ['data-files-tree'],

  /**
   * Reference to File - root of the filesystem showed in tree.
   * @type File
   */
  rootDir: null,

  didInsertElement() {
    this.bindResizeHandler();
    $(window).on('resize', this.get('updateResizeHandlerPositionFun'));
  },

  willDestroyElement() {
    $(window).off('resize', this.get('updateResizeHandlerPositionFun'));
  },

  updateResizeHandlerPositionFun: Ember.computed(function() {
    const $resizeHandler = $('#data-sidebar-resize-handler');
    const $secondarySidebar = $('.secondary-sidebar');
    return function() {
      $resizeHandler.css('left', $secondarySidebar.width() - $resizeHandler.width()/2);
    };
  }),

  /**
   * Enables "resize handler" - a invisible separator, that allows to change
   * width of secondary-sidebar.
   */
  bindResizeHandler() {
    const handleSelector = '#data-sidebar-resize-handler';
    const $resizeHandler = $(handleSelector);
    // resize handler is positioned absolutely to main-content
    this.get('updateResizeHandlerPositionFun')();
    const self = this;
    $('.secondary-sidebar').resizable({
      handleSelector: handleSelector,
      resizeHeight: false,
      onDrag(e, $el, newWidth/*, newHeight, opt*/) {
        $resizeHandler.css('left', newWidth - $resizeHandler.width()/2);
        self.get('eventsBus').trigger('secondarySidebar:resized');
      },
      onDragEnd(/*e, $el, opt*/) {
        self.get('updateResizeHandlerPositionFun')();
        self.get('eventsBus').trigger('secondarySidebar:resized');
      },
    });
  },

  actions: {
    /** Typically invoked by actions passed up from tree nodes */
    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    }
  }
});
