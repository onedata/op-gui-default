/**
 * Controller used to watch if model of route changed - and then change active
 * transfers menu option.
 * @module controllers/transfers/show
 * @author Jakub Liput
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
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
   * - `list_tab` (string) - id of transfers table list to open
   */
  queryParams: ['list_tab', 'file_id'],
    
  list_tab: undefined,
    
  listTab: computed.reads('list_tab'),
  fileId: computed.reads('file_id'),
    
  /**
   * Reset only some of the params
   */
  resetQueryParams() {
    this.setProperties({
      list_tab: undefined,
      file_id: undefined,
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
    changeListTab(listTab) {
      this.set('list_tab', listTab);
    },
    closeFileTab() {
      this.set('file_id', undefined);
    },
    resetQueryParams() {
      this.resetQueryParams();
    },
  },
});
