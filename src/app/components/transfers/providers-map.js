/**
 * Map of providers, their transfers and provider transfer states.
 *
 * @module components/transfers/providers-map
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

import _ from 'lodash';

const {
  Component,
  computed,
  A,
} = Ember;

export default Component.extend({
  classNames: ['transfers-providers-map'],
  
  /**
   * @virtual
   * Cannot be null or undefined!
   * @type {Array<Provider>}
   */
  providers: undefined,
  
  /**
   * @virtual
   * Ids of providers that are destination of transfers
   * @type {Array<string>}
   */
  destinationProviderIds: undefined,
  
  /**
   * @virtual
   * Ids of providers that are sources of transfers
   * @type {Array<string>}
   */
  sourceProviderIds: undefined,
  
  /**
   * @virtual
   * Collection of [src, dest] provider IDs to create lines on map.
   * Only one for pair!
   * @type {Array<Array[string,string]>}
   */
  providerTransferConnections: undefined,
  
  /**
   * Maps provider id => Provider model
   * @type {Ember.ComputedProperty<object>}
   */
  providersMap: computed('providers.[]', function () {
    const providers = this.get('providers');
    if (providers) {
      return _.zipObject(_.map(providers, 'id'), providers);
    } else {
      console.warn('component:transfers/providers-map: providers list is null');
      return null;
    }
  }),
  
  init() {
    this._super(...arguments);
    this.setProperties({
      _providersInfoCache: A(),
    });
  },
});
