import Ember from 'ember';
import _ from 'lodash';

import spaceTransfers from 'op-worker-gui/utils/transfers/space-transfers';

const {
  computed,
  Component,
} = Ember;

export default Component.extend({
  classNames: ['transfer-throughput-distribution'],

  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,
  
  /**
   * @virtual 
   * @type {Array<ProviderTransfer>}
   */
  providerTransfers: undefined,
  
  transfersDirection: 'out',

  providersMap: computed('providers.[]', function () {
    const providers = this.get('providers');
    return _.zipObject(_.map(providers, 'id'), providers);
  }),
  
  /**
   * FIXME: provide this collection - multiple for single source
   * only one for destination!
   * @type {Array<SpaceInputTransfer|SpaceOutputTransfer>}
   */
  spaceTransfers: computed('transfersDirection', 'providerTransfers.[]', function () {
    const {
      transfersDirection,
      providerTransfers,
    } = this.getProperties('transfersDirection', 'providerTransfers');
    return spaceTransfers(providerTransfers, transfersDirection);
  }),
  
  actions: {
    toggleDirection() {
      let direction = this.get('transfersDirection');
      direction = _.startsWith(direction, 'o') ? 'in' : 'out';
      this.set('transfersDirection', direction);
    },
  },
});
