/**
 * Adds sticky header to models-table component
 * 
 * @module components/one-models-table
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ModelsTable from 'ember-models-table/components/models-table';
import ModelsTableLayout from 'ember-models-table/templates/components/models-table';
import 'npm:sticky-table-headers';

const {
  observer,
} = Ember;

export default ModelsTable.extend({
  layout: ModelsTableLayout,
  
  /**
   * @virtual optional
   * @type {number}
   */
  stickyOffsetTop: 0,
  
  /**
   * @virtual
   * @function (boolean) => undefined
   */
  stickyHeaderChanged: () => {},
   
  updateHeaderOffset: observer('stickyOffsetTop', function () {
    this.$().stickyTableHeaders({ fixedOffset: this.get('stickyOffsetTop') });
  }),
  
  _stickyHeaderChanged(state) {
    this.get('stickyHeaderChanged')(state);
  },
  
  didInsertElement() {
    this._super(...arguments);
    
    this.$().stickyTableHeaders({
      scrollableArea: $('#content-scroll'),
      fixedOffset: this.get('stickyOffsetTop'),
      cacheHeaderHeight: true,
    });
    
    this.$().on(
      'enabledStickiness.stickyTableHeaders',
      () => this._stickyHeaderChanged(true)
    );
    this.$().on(
      'disabledStickiness.stickyTableHeaders',
      () => this._stickyHeaderChanged(false)
    );
  },
});
