import Ember from 'ember';

import _ from 'lodash';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({
  /**
   * @virtual
   * FIXME: each provider on map info
   */
  providers: undefined,
  
  providersMap: computed('providers.[]', function () {
    const providers = this.get('providers');
    return _.zipObject(_.map(providers, 'id'), providers);
  }),


  /**
   * @virtual
   * FIXME: provide collection of { src, dest } to create lines on map
   * only one for pair!
   */
  providerTransferConnections: undefined,
});
