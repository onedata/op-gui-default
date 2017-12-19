/**
 * Controller used to watch if model of route changed - and then change active
 * transfers menu option.
 * @module controllers/transfers/show
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Controller,
  inject: { service },
  run,
  computed,
} = Ember;

export default Controller.extend({
  secondaryMenu: service(),
  
  /**
   * Query parameters used for onedata.transfers.show route:
   * - `sort_by` (string) - key of transfers table column to sort by default
   *    on entering the view; see `live-stats-table` component for column names;
   *    eg. "path" to sort by file path
   * - `selected_transfers` (string) - list of transfers ids separated by `,`
   *    that will be automatically opened, blink and be scrolled to on entering
   *    the view; eg. "transferid1,transferid2"
   */
  queryParams: ['sort_by', 'selected_transfers'],
    
  /**
   * @type {Ember.ComputedProperty<Array<String>>}
   */
  selectedTransferIds: computed('selected_transfers', function () {
    const selected_transfers = this.get('selected_transfers');
    if (selected_transfers) {
      return selected_transfers.toString().split(',');
    }
  }),
  
  sortBy: computed.reads('sort_by'),
  
  resetQueryParams() {
    this.setProperties({
      sort_by: undefined,
      selected_transfers: undefined,
    });
  },

  changeMenuActiveItem() {
    this.set('secondaryMenu.activeItem', this.get('model'));

    // TODO: use property binding
    run.scheduleOnce('afterRender', this, function() {
      $('nav.secondary-sidebar').addClass('visible');
    });
  },

  onSpaceChange: function() {
    if (this.get('model')) {
      this.changeMenuActiveItem();
    }
  },

  actions: {
    resetQueryParams() {
      this.resetQueryParams();
    },
  },
});
