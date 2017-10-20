import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

import providerTransfers from 'op-worker-gui/utils/transfers/provider-transfers';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';

export default Component.extend({
  /**
   * @virtual
   * @type {Space}
   */
  space: undefined,

  /**
   * Collection of Transfer model
   */
  transfers: computed.reads('space.transferList.list.content'),
  // FIXME: transfers loading (private)
  // FIXME: transfers error (private)

  providers: computed.reads('space.providerList.list.content'),
  // FIXME: providers loading (important: yielded)
  // FIXME: providers error (important: yielded)

  /**
   * See `util:transfers/provider-input-transfers` for type def. and generation
   * @type {Array<ProviderInputTransfer>}
   */
  providerTransfers: computed('transfers.[]', function () {
    return providerTransfers(this.get('transfers'));
  }),

  /**
   * Collection of connection between two providers (for map display)
   * Order in connection is random; each pair can occur once.
   * See `util:transfers/provider-transfer-connections`
   * `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
   * @type {Array<ProviderTransferConnection>}
   */
  providerTransferConnections: computed('providerTransfers', function () {
    return providerTransferConnections(this.get('providerTransfers'));
  }),
});
