import Ember from 'ember';

import _ from 'lodash';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({
  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,
  
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
    return _.zipObject(_.map(providers, 'id'), providers);
  }),
});
