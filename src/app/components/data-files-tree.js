import Ember from 'ember';
import _ from 'lodash';

const {
  run,
  inject,
  computed
} = Ember;

/**
 * Container for data-files-tree-list(s).
 *
 * Sends actions:
 * - openDirInBrowser(file) - open dir for browsing
 * @module components/data-files-tree
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  fileBrowser: inject.service(),
  fileSystemTree: inject.service(),
  eventsBus: inject.service(),
  commonModals: inject.service(),
  session: inject.service(),
  
  classNames: ['data-files-tree'],

  /**
   * Reference to File - root of the filesystem showed in tree.
   * @type File
   */
  rootDir: null,

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  currentProviderId: computed.alias('session.sessionDetails.providerId'),

  /**
   * True if the current provider supports selected space
   * @type {Ember.ComputedProperty<boolean|undefined>}
   */
  currentProviderSupport: computed(
    'currentProviderId',
    'fileSystemTree.selectedSpace.providerList.list',
    function () {
      const providerIdList =
        this.get('fileSystemTree.selectedSpace.providerList.list');
      const currentProviderId = this.get('currentProviderId');
      if (providerIdList) {
        return _.includes(providerIdList, currentProviderId);
      }
    }
  ),
  
  didInsertElement() {
    this._super(...arguments);
    this.bindResizeHandler();
  },

  willDestroyElement() {
    $(window).off('resize', this.get('updateResizeHandlerPositionFun'));
  },

  updateResizeHandlerPositionFun: computed(function() {
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
    run.scheduleOnce('afterRender', this, function() {
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
      $(window).on('resize', this.get('updateResizeHandlerPositionFun'));
    });
  },

  actions: {
    /** Typically invoked by actions passed up from tree nodes */
    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    },
    rootDataDistribution() {
      this.get('fileSystemTree.selectedSpace.providerList').then(() => {
        this.get('commonModals').openModal('dataDistribution', {
          fileForChunks: this.get('rootDir'),
          currentProviderSupport: this.get('currentProviderSupport'),
          space: this.get('fileSystemTree.selectedSpace'),
        });
      });
    },
  }
});
