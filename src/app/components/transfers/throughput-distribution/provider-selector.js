/**
 * Renders provider selector for throughput-distribution chart.
 * 
 * @module components/transfers/throughput-distribution/provider-selector
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  computed,
  inject: {
    service,
  }
} = Ember;

const I18N_PREFIX =
  'components.transfers.throughputDistribution.providerSelector.';

export default Ember.Component.extend({
  classNames: ['throughput-provider-selector'],

  i18n: service(),

  /**
   * Null means "all providers"
   * @type {string|null}
   */
  selectedProviderId: null,

  /**
   * @type {object}
   */
  providersNames: Object.freeze({}),

  /**
   * @virtual
   * @type {function}
   */
  selectProvider: () => {},

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  selectedProviderName: computed('selectedProviderId', 'providersNames', function () {
    const {
      i18n,
      selectedProviderId,
      providersNames,
    } = this.getProperties('i18n', 'selectedProviderId', 'providersNames');
    return selectedProviderId ?
      providersNames[selectedProviderId] :
      i18n.t(I18N_PREFIX + 'allProviders');
  })
});
