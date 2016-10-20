import Ember from 'ember';

// FIXME: jsdoc
export default Ember.Component.extend({
  classNames: ['data-files-list-loader'],
  attributeBindings: ['style'],

  /**
   * To inject.
   * From which file-row the loader should start
   * @type {Number}
   */
  startRow: null,

  /**
   * Files table element (table)
   * @private
   * @type {jQuery}
   */
  $filesTable: null,

  style: Ember.computed('top', '$filesTable', function() {
    let style = 'display: none;';

    const startRow = this.get('startRow');
    if (this.get('$filesTable') && startRow != null) {
      const top = this.get('top');
      if (top != null) {
        style = `display: block; top: ${top}px;`;
      }
    }
    return Ember.String.htmlSafe(style);
  }),

  __stickyFun: null,

  bindSticky() {
    let $spinner = this.$().find('.spinner-container');
    $spinner.sticky({topSpacing: 80});

    let updateFun = function() {
      $spinner.sticky('update');
    };

    this.set('__stickyFun', updateFun);
    this.$().closest('#content-scroll').on('scroll', updateFun);
  },

  unbindSticky() {
    this.$().closest('#content-scroll').off('scroll', this.get('__stickyFun'));
  },

  didInsertElement() {
    this.set('$filesTable', this.$().closest('.data-files-list').find('.files-table'));
    // TODO: Currently sticky spinner is disabled due to bugs
    // this.bindSticky();
  },

  willDestroyElement() {
    // TODO: Currently sticky spinner is disabled due to bugs
    // this.unbindSticky();
  },

  top: Ember.computed('startRow', function() {
    let startRow = this.get('startRow');
    let $filesTable = this.get('$filesTable');
    let $row = $filesTable.find(`.file-row-index-${startRow}`);
    if ($row.length === 1) {
      return $row.position().top;
    } else {
      // if startRow is not rendered yet for loader, use last row's bottom
      // so we have a small bottom loader no matter if there are loading rows
      $row = $filesTable.find(`.file-row-index-${startRow-1}`);
      if ($row.length === 1) {
        return $row.position().top + $row.height();
      }
    }
  }),

  deb: Ember.observer('startRow', 'top', 'lastRowBottom', 'rowsCount', function() {
    let p = this.getProperties('startRow', 'top', 'lastRowBottom', 'rowsCount');
    console.debug(`sr: ${p.startRow}, srt: ${p.top}, lastRowBottom: ${p.lastRowBottom}, rc: ${p.rowsCount}`);
  }),

});
