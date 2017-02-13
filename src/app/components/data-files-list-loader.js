import Ember from 'ember';

const {
  run,
  computed,
  String :{
    htmlSafe
  }
} = Ember;

/**
 * A loader element that covers all files that are new in last ``fetchMoreFiles``
 * request. It covers always from previous loaded files list to bottom of table.
 * @module components/data-files-list-loader
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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

  style: computed('top', '$filesTable', function() {
    let style = 'display: none;';

    const startRow = this.get('startRow');
    if (this.get('$filesTable') && startRow != null) {
      const top = this.get('top');
      if (top != null) {
        style = `display: block; top: ${top}px;`;
      }
    }
    return htmlSafe(style);
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
    this._super(...arguments);
    run.scheduleOnce('afterRender', this, function() {
      this.set('$filesTable', this.$().closest('.data-files-list').find('.files-table'));
      // TODO: Currently sticky spinner is disabled due to bugs
      // this.bindSticky();
    });
  },

  willDestroyElement() {
    // TODO: Currently sticky spinner is disabled due to bugs
    // this.unbindSticky();
  },

  top: computed('startRow', '$filesTable', function() {
    let startRow = this.get('startRow');
    let $filesTable = this.get('$filesTable');
    let row = $filesTable.find('.file-row')[startRow];
    if (row) {
      return row.offsetTop;
    } else {
      // if startRow is not rendered yet for loader, use last row's bottom
      // so we have a small bottom loader no matter if there are loading rows
      row = $filesTable.find('.file-row')[startRow-1];
      if (row) {
        return row.offsetTop + row.offsetHeight;
      }
    }
  }),
});
