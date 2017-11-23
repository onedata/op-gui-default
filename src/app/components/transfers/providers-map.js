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
  destinationProviderIds: A(['p1', 'p3']),
  
  /**
   * @virtual
   * Ids of providers that are sources of transfers
   * @type {Array<string>}
   */
  // FIXME:
  sourceProviderIds: A(['p2', 'p3']),
  
  /**
   * @virtual
   * Collection of [src, dest] provider IDs to create lines on map.
   * Only one for pair!
   * @type {Array<Array[string,string]>}
   */
  // providerTransferConnections: undefined,
  providerTransferConnections: A([
    ['p1', 'p2'],
  ]),
  
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
