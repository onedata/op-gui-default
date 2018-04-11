/**
 * Renders list of on-the-fly transfers.
 * 
 * @module components/transfers/on-the-fly-list
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

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
   * Objects that represents each list element - on the fly transfer
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  listRecords: computed('transfers', 'providers', function () {
    const {
      transfers,
      providers,
    } = this.getProperties('transfers', 'providers');
    return transfers.map(transfer => {
      const provider =
        _.find(providers, p => get(p, 'id') === get(transfer, 'destination'));
      return {
        transfer,
        provider,
      };
    });
  }),
});
