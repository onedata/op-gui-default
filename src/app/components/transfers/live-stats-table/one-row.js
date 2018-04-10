// FIXME: jsdoc

import Ember from 'ember';

const {
  Component,
  computed,
  String: { htmlSafe },
} = Ember;

const rowHeight = 73;
const paddingOffsetTop = 16;

export default Component.extend({
  tagName: '',
  
  visibleColumnsCount: computed('processedColumns.length', function () {
    return this.get('processedColumns.length') - 1;
  }),
  
  tdStyle: computed('index', 'record.listIndex', function getTdStyle() {
    if (this.get('index') === 0) {
      return htmlSafe(
        `padding-top: ${this.get('record.listIndex') * rowHeight + paddingOffsetTop}px;`
      );
    } else {
      return htmlSafe();
    }
  }),
  
  actions: {
    expandRow() {
      return this.get('expandRow')(...arguments);
    },
    collapseRow() {
      return this.get('collapseRow')(...arguments);
    },
  }
});
