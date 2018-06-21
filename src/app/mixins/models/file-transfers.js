/**
 * Methods for fetching transfers list for the file
 * 
 * @module mixins/models/file-transfers
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import computedFileTransfersList from 'op-worker-gui/utils/computed-file-transfers-list';

const {
  RSVP: { Promise },
} = Ember;

export default Ember.Mixin.create({
  /**
   * @type {boolean}
   */
  limitReached: false,
  
  /**
   * Contains transfers records associated with this file
   * @type {Ember.ComputedProperty<FakeListRecordRelation>}
   */
  transferList: computedFileTransfersList('id'),
  
  /**
   * Fetch transfer records for this file
   * @param {string} fileId should be set to this file ID
   */
  fetchTransfers(fileId) {
    const {
      oneproviderServer,
      store,
    } = this.getProperties('oneproviderServer', 'store');
    return oneproviderServer.getTransfersForFile(fileId, 'ids')
      .then(({ ongoing, ended }) => {
        // FIXME: do not fetch unnecessary!
        console.log('fetch');
        const list = [...ongoing, ...ended];
        return Promise.all(list.map(transferId => store.findRecord('transfer', transferId)));
      });
  },
});
