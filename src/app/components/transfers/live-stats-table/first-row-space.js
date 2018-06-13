/**
 * A first pseudo-row of transfers table/list that adds spacing for upper
 * transfers items, that are currently not rendered
 *
 * @module components/transfers/live-stats-table/first-row-space
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
  String: { htmlSafe },
} = Ember;

const rowHeight = 73;
const offsetScreenTop = 130;

export default Component.extend({
  tagName: 'tr',
  classNames: ['first-row-space'],

  firstRowListIndex: undefined,

  /**
   * @virtual
   * @type {number}
   */
  processedColumnsCount: undefined,
  
  visibleColumnsCount: computed('processedColumnsCount', function () {
    return this.get('processedColumnsCount') - 1;
  }),

  virtualRowsNumber: computed(
    'firstRowListIndex',
    'justOpened',
    function getVirtualRowsNumber() {
      const {
        firstRowListIndex,
        justOpened,
      } = this.getProperties(
        'firstRowListIndex',
        'justOpened'
      );
      
      return (
        (!firstRowListIndex || firstRowListIndex < 0) && justOpened ?
        1 : firstRowListIndex
      );
    }),
  
  tdStyle: computed('virtualRowsNumber', function getTdStyle() {
    return htmlSafe(
      `height: ${this.get('virtualRowsNumber') * rowHeight}px;`
    );
  }),
  
  middleStickyClass: computed('middleStickyVisible', 'firstRowListIndex', function getTdClass() {
    return htmlSafe(
      this.get('middleStickyVisible') && this.get('firstRowListIndex') ? '' : 'middle-sticky-hide'
    );
  }),

  didInsertElement() {
    this.updateSpinner();
    $('#content-scroll').on('scroll.firstRowSpace', () => this.updateSpinner());
  },

  willDestroyElement() {
    $('#content-scroll').off('.firstRowSpace');
  },
  
  updateSpinner() {
    const $row = this.$();
    const rowStart = $row.offset().top;
    const rowEnd = rowStart + $row.height();
    const middleSticky = this.$('.middle-sticky');
    if (this.get('firstRowListIndex') && rowEnd > offsetScreenTop + rowHeight) {
      this.set('middleStickyVisible', true);
      const screenBottom = $(window).height();
      const newTop =
        (Math.max(offsetScreenTop, rowStart) + Math.min(screenBottom, rowEnd)) / 2;
      const td = middleSticky.parents('.first-row-inner');
      const tdLeft = td.offset().left;
      middleSticky.css({
        top: newTop,
        left: tdLeft,
        width: td.width(),
      });
    } else {
      this.set('middleStickyVisible', false);
    }
  },
});
