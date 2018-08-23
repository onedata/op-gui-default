/**
 * A table/list row representing single transfer
 *
 * @module components/transfers/live-stats-table/transfer-row
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import $ from 'jquery';

const {
  Component,
  computed,
} = Ember;

const emptyFun = () => {};

export default Component.extend({
  tagName: '',
  
  /**
   * @virtual optional
   * @type {function|undefined}
   */
  expandRow: undefined,
  
  /**
   * @virtual optional
   * @type {function|undefined}
   */
  collapseRow: undefined,
  
  visibleColumnsCount: computed('processedColumns.length', function () {
    return this.get('processedColumns.length') - 1;
  }),
  
  init() {
    this._super(...arguments);
    if (!this.get('expandRow')) {
      this.set('expandRow', emptyFun);
    }
    if (!this.get('collapseRow')) {
      this.set('collapseRow', emptyFun);
    }
  },

  /**
   * Returns true if click should be ignored
   * @param {jQuery.Event} event
   * @returns {boolean}
   */
  shouldIgnoreClick(event) {
    // ignore dots-menu
    return $(event.target).closest('.dots-menu').length > 0;
  },
  
  actions: {
    expandRow(transferIndex, event) {
      if (this.shouldIgnoreClick(event)) {
        return;
      }
      /** @type {function|undefined} */
      const expandRow = this.get('expandRow');
      return expandRow ? expandRow(...arguments) : null;
    },
    collapseRow(transferIndex, event) {
      if (this.shouldIgnoreClick(event)) {
        return;
      }
      /** @type {function} */
      const collapseRow = this.get('collapseRow');
      return collapseRow ? collapseRow(...arguments) : null;
    },
  }
});
