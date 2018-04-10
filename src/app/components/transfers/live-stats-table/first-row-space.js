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
  processedCoumnsCount: undefined,
  
  visibleColumnsCount: computed('processedColumnsCount', function () {
    return this.get('processedColumnsCount') - 1;
  }),

  tdStyle: computed('firstRowListIndex', function getTdStyle() {
    return htmlSafe(
      `height: ${this.get('firstRowListIndex') * rowHeight}px;`
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

  willDestroyElement() {
    $('#content-scroll').off('.firstRowSpace');
  },
});
