/**
 * Gives an action to change space transfer route
 * 
 * @module controllers/onedata/transfers
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Controller
} = Ember;

export default Controller.extend({
  actions: {
    /** Show transfers for Space */
    goToTransfersForSpace(space) {
      return this.transitionToRoute('onedata.transfers.show', space);
    },
    
    resetShowQueryParams() {
      return this.transitionToRoute({
        queryParams: {
          selected_transfers: undefined,
          sort_by: undefined,
        },
      });
    },
  }
});
