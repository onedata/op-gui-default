// FIXME: jsdoc

import Ember from 'ember';

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
  
  actions: {
    expandRow() {
      /** @type {function|undefined} */
      const expandRow = this.get('expandRow');
      return expandRow ? expandRow(...arguments) : null;
    },
    collapseRow() {
      /** @type {function} */
      const collapseRow = this.get('collapseRow');
      return collapseRow ? collapseRow(...arguments) : null;
    },
  }
});
