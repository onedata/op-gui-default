/**
 * Controller used to redirect to default transfers for space on model load.
 * Not using afterModel of route, because there were some problems
 * (afterModel was invoked before model load).
 * @module controllers/transfers/index
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import getDefaultSpace from 'op-worker-gui/utils/get-default-space';

const {
  Controller,
  inject: { service },
  RSVP: { resolve },
  computed,
} = Ember;

export default Controller.extend({
  secondaryMenu: service(),
  fileSystemTree: service(),
  session: service(),
  
  providerId: computed.reads('session.sessionDetails.providerId'),
  
  goToDefaultTransfersForSpace() {
    console.debug(`controller:onedata.transfers.index: Will try to go to default tr. for space`);
    return this._getDefaultSpace(this.get('model'))
      .then(defaultSpace => {
        if (defaultSpace) {
          return this.replaceRoute('onedata.transfers.show', defaultSpace);
        } else {
          console.debug(`controller:onedata.transfers.index: No space to go`);
        }
      });
  },

  /**
   * Try to use space that is loaded into secondary menu before using default one
   * @param {Array<Space>} model
   * @param {string} providerId
   * @returns {Promise<models/DataSpace>}
   */
  _getDefaultSpace(model) {
    const activeSpace = this.get('secondaryMenu.activeSpace');
    return activeSpace ? resolve(activeSpace) :
      getDefaultSpace(model, this.get('providerId'));
  },
  
  /**
   * Observe for a default space to appear in spaces route model (which is a spaces list).
   * When found - show this space.
   * This is a workaround for afterModel, which does not recieve ready spaces list.
   */
  onModelChange: Ember.observer('model.@each.isLoaded', 'model.isUpdating',
    function() {
      if (this.get('isActive')) {
        return this.goToDefaultTransfersForSpace();
      }
    }
  )
});
