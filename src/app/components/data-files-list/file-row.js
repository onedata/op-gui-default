import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',
  classNames: ['first-level', 'file-row'],
  classNameBindings: [
    'highlightClass',
    'isDownloading:selection-background-pulse',
    'indexClass',
    'isNewlyCreated:notice-background-pulse'
  ],

  highlightClass: Ember.computed('file.isSelected', 'file.isEditingMetadata', function() {
    return this.get('file.isSelected') && 'active' ||
      this.get('file.isEditingMetadata') && 'metadata-opened' ||
      '';
  }),

  indexClass: Ember.computed('listIndex', function() {
    return `file-row-index-${this.get('listIndex')}`;
  }),

  isNewlyCreated: Ember.computed.alias('file.isNewlyCreated'),

  /**
   * To inject - a file that the row represents
   * @type File
   */
  file: null,

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
  checkVisibilityFun: Ember.computed(function() {
    const self = this;
    return function() {
      if (self.$().visible()) {
        self.sendAction('onAppear', self.get('listIndex'));
      }
    };
  }),

  didInsertElement() {
    if (this.get('watchAppear')) {
      this.enableVisibilityChecking();
    }
  },

  willDestroyElement() {
    if (this.get('watchAppear')) {
      this.disableVisibilityChecking();
    }
  },

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

  toggleVisibilityChecking: Ember.observer('watchAppear', function() {
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
    }
  }
});
