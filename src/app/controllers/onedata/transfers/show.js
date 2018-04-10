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
   * - `selected_transfers` (string) - list of transfers ids separated by `,`
   *    that will be automatically opened, blink and be scrolled to on entering
   *    the view; eg. "transferid1,transferid2"
   * - `list_tab` (string) - id of transfers table list to open
   */
  queryParams: ['selected_transfers', 'list_tab'],
    
  /**
   * @type {Ember.ComputedProperty<Array<String>>}
   */
  selectedTransferIds: computed('selected_transfers', function () {
    const selected_transfers = this.get('selected_transfers');
    if (selected_transfers) {
      return selected_transfers.toString().split(',');
    }
  }),
  
  listTab: computed.alias('list_tab'),
    
  /**
   * Reset only some of the params
   */
  resetQueryParams() {
    this.setProperties({
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
