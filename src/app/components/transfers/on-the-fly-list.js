/**
 * Renders list of on-the-fly transfers.
 * 
 * @module components/transfers/on-the-fly-list
 * @author Michal Borzecki, Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  get,
  computed,
} = Ember;

export default Ember.Component.extend({
  /**
   * Array of transfers
   * @type {Array<OnTheFlyTransfer>}
   */
  transfers: undefined,

  /**
   * Providers
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,
  
  /**
   * Colors used to color each providers' series
   * @virtual
   * @type {Object}
   */
  providersColors: undefined,

  /**
   * List of provider IDs of the opened on-the-fly transfers charts.
   * @virtual
   * @type {Ember.Array<string>}
   */
  openedProviderIds: undefined,
  
  /**
   * Notify parent about opened provider chart
   * @virtual
   * @type {Function} `(string) => any`
   */
  toggleProviderId: () => {},
  
  /**
   * Array of objects that represents each list element - on the fly transfer
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  listRecords: computed('transfers', 'providers', function () {
    const {
      transfers,
      providers,
    } = this.getProperties('transfers', 'providers');
    return transfers.map(transfer => {
      const provider =
        providers.filter(p => get(p, 'id') === get(transfer, 'replicatingProvider'))[0];
      return {
        transfer,
        provider,
      };
    }).sort((a, b) =>
      get(a.provider, 'name').localeCompare(get(b.provider, 'name'))
    );
  }),
    
  actions: {
    itemToggled(providerId, opened) {
      const toggleProviderId = this.get('toggleProviderId');
      toggleProviderId(providerId, opened);
    },
  },
});
