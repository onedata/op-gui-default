import Ember from 'ember';

const {
  computed,
  run,
  observer,
  RSVP: {
    Promise
  },
  String: {
    htmlSafe
  }
} = Ember;

/**
 * Represents a row with file in files list.
 * 
 * ## Component actions sent
 * - fileClicked(file: File, ctrlOrCmdKey: Boolean, shiftKey: Boolean)
 * 
 * @module components/file-row
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'tr',
  classNames: ['first-level', 'file-row'],
  classNameBindings: [
    'highlightClass',
    'isDownloading:selection-background-pulse',
    'isNewlyCreated:notice-background-pulse'
  ],

  fileLabelStyle: computed('labelMaxWidth', function() {
    let style = `max-width: ${this.get('labelMaxWidth')}px;`;
    return htmlSafe(style);
  }),

  highlightClass: computed(
    'file.{isSelected,isEditingMetadata,isShowingInfo}',
    function highlightClass() {
      let classes = '';
      if (this.get('file.isSelected')) {
        classes += 'active';
      }
      if (this.get('file.isEditingMetadata')) {
        classes += ' metadata-opened';
      }
      if (this.get('file.isShowingInfo')) {
        classes += ' info-opened';
      }
      return classes;
    }
  ),

  isNewlyCreated: computed.alias('file.isNewlyCreated'),

  /**
   * To inject - a file that the row represents
   * @type File
   */
  file: null,

  /**
   * To inject, optional.
   * A label that should be shown. If is null, use file.name.
   * For final computed label in GUI, see: ``displayedFileLabel``.
   * @type {String}
   */
  label: null,

  /**
   * To inject, optional.
   * A distinguishable ID of provider to display
   * for distinguishing conflicting files.
   * @type {String}
   */
  providerLabel: computed.alias('file.conflictLabel'),

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

  /**
   * To inject.
   * What position on list has this row.
   * Should be injected by files-list.
   * @type {Number}
   */
  listIndex: null,

  /**
   * To inject.
   * Should we send "appeared" action if this element will be visible?
   * Should not be set in runtime, only on init! (because it may cause unexpected results)
   * @type {Boolean}
   */
  watchAppear: false,

  /**
   * To inject.
   * @type {Number}
   */
  labelMaxWidth: null,

  visibilityCheckIntervalSeconds: 5,

  /**
   * Used only if ``watchAppear`` is enabled.
   * ID of interval that checks visibility of element periodically.
   * @type {Number}
   */
  visibilityCheckIntervalId: null,

  /**
   * True if ``enableVisibilityChecking`` was already invoked without disabling it.
   * @type {Boolean}
   */
  _visibilityCheckingBound: false,

  /**
   * Returns a function that checks if this element is visible and if, sends action.
   * @type {Function}
   */
  checkVisibilityFun: computed(function() {
    const self = this;
    return function() {
      if (self.$().visible()) {
        self.sendAction('onAppear', self.get('listIndex'));
      }
    };
  }),

  /**
   * A label that is actually displayed in GUI.
   * @type {String}
   */
  displayedFileLabel: computed('label', 'file.name', function() {
    return this.get('label') || this.get('file.name');
  }),

  didInsertElement() {
    this._super(...arguments);
    run.scheduleOnce('afterRender', this, function() {
      if (this.get('watchAppear')) {
        this.enableVisibilityChecking();
      }
    });
  },

  willDestroyElement() {
    if (this.get('watchAppear')) {
      this.disableVisibilityChecking();
    }
    this.set('isNewlyCreated', false);
  },

  click(clickEvent) {
    this.sendAction(
      'fileClicked',
      this.get('file'),
      clickEvent.ctrlKey || clickEvent.metaKey,
      clickEvent.shiftKey
    );
  },

  doubleClick() {
    if (this.get('file.isDir')) {
      this.sendAction('openDirInBrowser', this.get('file'));
    } else {
      this.set('isDownloading', true);
      const p = new Promise((resolve, reject) => {
        this.sendAction('downloadFile', this.get('file'), resolve, reject);
      });
      p.finally(() => this.set('isDownloading', false));
    }
  },

  toggleVisibilityChecking: observer('watchAppear', function() {
    if (this.get('watchAppear')) {
      this.enableVisibilityChecking();
    } else {
      this.disableVisibilityChecking();
    }
  }),

  /**
   * Bind for some events and if they occur checks visibility of this element with ``checkVisibilityFun``.
   * NOTE that binding is invoked only if ``_visibilityCheckingBound`` is false to prevent multiple binding.
   */
  enableVisibilityChecking() {
    if (!this.get('_visibilityCheckingBound')) {
      this.set('_visibilityCheckingBound', true);
      const $contentScroll = $('#content-scroll');
      const fun = this.get('checkVisibilityFun');
      $contentScroll.on('mousewheel', fun);
      $(window).on('resize', fun);
      // in case, user scrolled the panel using scroll bar or page down or something
      this.set(
        'visibilityCheckIntervalId',
        window.setInterval(fun, this.get('visibilityCheckIntervalSeconds')*1000)
      );
      fun();
    }
  },

  /**
   * Unbinds events that are binded with ``enableVisibilityReporting``
   */
  disableVisibilityChecking() {
    this.set('_visibilityCheckingBound', false);
    const $contentScroll = $('#content-scroll');
    const fun = this.get('checkVisibilityFun');
    $contentScroll.off('mousewheel', fun);
    $(window).off('resize', fun);
    window.clearInterval(this.get('visibilityCheckIntervalId'));
  },

  actions: {
    shareFile(file) {
      this.sendAction('openFileShareModal', file || this.get('file'));
    },
    toggleFileMetadata(file) {
      this.sendAction('toggleFileMetadata', file || this.get('file'));
    },
    toggleFileInfo(file) {
      this.sendAction('toggleFilesInfo', [ file || this.get('file') ]);
    }
  }
});
