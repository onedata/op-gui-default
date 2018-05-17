/**
 * Transfer record wrapper for transfers table/list item 
 *
 * @module utils/transfer-table-record
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import computedPipe from 'ember-cli-onedata-common/utils/ember/computed-pipe';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';

const {
  Object: EmberObject,
  computed,
  get,
  set,
  setProperties,
} = Ember;

const START_END_TIME_FORMAT = 'D MMM YYYY H:mm:ss';

export default EmberObject.extend({
  destinationUnknownText: 'unknown',
  
  /**
   * @virtual
   * @type {Array<string>}
   */
  selectedTransferIds: undefined,
  
  /**
   * @virtual
   * @type {models/Transfer}
   */
  transfer: undefined,
  
  /**
   * @virtual
   * @type {ArraySlice<models/Transfer>}
   */
  transfers: undefined,
  
  /**
   * @virtual
   * @type {Array<models/Provider>}
   */
  providers: undefined,
  
  /**
   * @virtual
   * @type {object}
   */
  providersColors: undefined,
  
  /**
   * @virtual
   * @type {number}
   */
  updaterId: undefined,
  
  transferId: computed.reads('transfer.id'),
  path: computed.reads('transfer.path'),
  fileType: computed.reads('transfer.fileType'),
  userName: computed.reads('transfer.userName'),
  scheduledAtComparable: computed.reads('transfer.scheduleTime'),
  startedAtComparable: computed.reads('transfer.startTime'),
  finishedAtComparable: computed.reads('transfer.finishTime'),
  transferredFiles: computed.reads('transfer.transferredFiles'),
  invalidatedFiles: computed.reads('transfer.invalidatedFiles'),
  status: computed.reads('transfer.status'),
  currentStatError: computed.reads('transfer.currentStatError'),
  type: computed.reads('transfer.type'),
  
  isLoading: computed('transfer.tableDataIsLoaded', 'isReloading', function () {
    return this.get('transfer.tableDataIsLoaded') === false || this.get('isReloading');
  }),
  
  transferIndex: computedPipe('transferId', getHash),
  scheduledAtReadable: computedPipe('scheduledAtComparable', timeReadable),
  startedAtReadable: computedPipe('startedAtComparable', timeReadable),
  finishedAtReadable: computedPipe('finishedAtComparable', timeReadable),
  totalBytesReadable: computedPipe('transfer.transferredBytes', bytesToString),
  
  initSelect: computed('selectedTransferIds.[]', 'transferId', function () {
    const {
      selectedTransferIds,
      transferId
    } = this.getProperties('selectedTransferIds', 'transferId');
    return _.includes(selectedTransferIds, transferId);
  }),
  
  listIndex: computed('transfer', 'transfers.@each.startIndex', function () {
    const transfer = this.get('transfer');
    const transfers = this.get('transfers');
    return transfers.indexOf(transfer) + get(transfers, '_start');
  }),
  
  destination: computed('providers.@each.name', 'transfer.destination', function () {
    const destinationId = this.get('transfer.destination');
    // invalidation transfer
    if (!destinationId) {
      return '-';
    }
    let destination = this.get('destinationUnknownText');
    const destProvider = destination ? _.find(this.get('providers'), provider => 
      get(provider, 'id') === destinationId
    ) : null;
    if (destProvider) {
      destination = get(destProvider, 'name');
    }
    return destination;
  }),
  
  init() {
    const transfer = this.get('transfer');
    if (get(transfer, 'isLoaded')) {
      const currentUpdaterId = this.get('updaterId');
      if (get(transfer, 'updaterId') !== currentUpdaterId) {
        setProperties(
          transfer, 
          {
            updaterId: currentUpdaterId,
            isReloading: true,
          }
        );
        transfer.reload()
          .then(() => transfer.belongsTo('currentStat').reload())
          .finally(() => set(transfer, 'isReloading', false));
      }
    } else if (!get(transfer, 'isLoading')) {
      transfer.store.findRecord('transfer', get(transfer, 'id'));
    }
  },
});

// https://stackoverflow.com/a/40958826
function getHash(input) {
  let hash = 0, len = input.length;
  for (let i = 0; i < len; i++) {
    hash = ((hash << 5) - hash) + input.charCodeAt(i);
    hash |= 0;
  }
  return hash;
}

function timeReadable(timestamp) {
  return (timestamp && moment.unix(timestamp).format(START_END_TIME_FORMAT)) || '-';
}
