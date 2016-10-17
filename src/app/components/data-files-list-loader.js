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
  // startRow: null,

  rowsCount: null,

  style: Ember.computed('startRowTop', 'lastRowBottom', '$filesTable', function() {
    let style;

    const startRow = this.get('startRow');
    if (this.get('$filesTable') && startRow != null) {
      const top = this.get('startRowTop');
      const bottom = this.get('lastRowBottom');
      if (top != null && bottom != null) {
        style = `display: block; top: ${top}px; bottom: ${bottom}px;`;
      }
    }
    style = 'display: none;';
    return Ember.String.htmlSafe(style);
  }),

  stickyFun: null,

  didInsertElement() {
    this.set('$filesTable', this.$().closest('.data-files-list').find('.files-table'));
    let $spinner = this.$().find('.spinner-container');
    $spinner.sticky({topSpacing: 80});

    let updateFun = function() {
      $spinner.sticky('update');
    };

    this.set('stickyFun', updateFun);
    this.$().closest('#content-scroll').on('scroll', updateFun);
  },

  willDestroyElement() {
    this.$().closest('#content-scroll').off('scroll', this.get('stickyFun'));
  },

  startRowTop: Ember.computed('rowsCount', 'rowIndex', function() {
    const $filesTable = this.get('$filesTable');
    const $row = $filesTable.find(`.file-row-index-${this.get('startRow')}`);
    if ($row.length === 1) {
      return $row.position().top;
    }
  }),

  lastRowBottom: Ember.computed('rowsCount', '$filesTable', function() {
    const $filesTable = this.get('$filesTable');
    if ($filesTable) {
      const $row = $filesTable.find('.file-row:last');
      if ($row.length === 1) {
        return $filesTable.height() - ($row.position().top + $row.height());
      }
    }
  }),

  deb: Ember.observer('startRow', 'startRowTop', 'lastRowBottom', 'rowsCount', function() {
    let p = this.getProperties('startRow', 'startRowTop', 'lastRowBottom', 'rowsCount');
    console.debug(`sr: ${p.startRow}, srt: ${p.startRowTop}, lastRowBottom: ${p.lastRowBottom}, rc: ${p.rowsCount}`);
  }),

});
