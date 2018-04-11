/**
 * A view component for onedata.transfers.show route
 *
 * @module components/show-space-transfers
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

const {
  Component,
  computed,
  isArray,
  get,
  inject: { service },
} = Ember;

export default Component.extend({
  classNames: ['show-space-transfers'],
  session: service(),
  
  //#region External properties
  
  /**
   * Space model, which transfers will be listed
   * @virtual
   * @type {Space}
   */
  space: undefined,
  
  /**
   * Name of transfer table column to sort by default (eg. path)
   * @type {string|undefined}
   */
  sortBy: undefined,
  
  /**
   * Ids of transfers that should be expanded, "blinked" and scrolled to
   * on entering view
   * @type {Array<string>|undefined}
   */
  selectedTransferIds: undefined,
  
  //#region External properties
  
  /**
   * Alias for Id of this provider - used for checking if transfers can be fetched
   * @type {Ember.ComputedProperty<string>}
   */
  sessionProviderId: computed.reads('session.sessionDetails.providerId'),
  
  /**
   * List of providers that support this space
   * @type {Ember.ComputedProperty<Ember.Array<Provider>>}
   */
  providers: computed.reads('space.providerList.queryList.content'),
  
  /**
   * True if transfers can be listed because space is supported by current
   * provider.
   * @type {Ember.ComputedProperty<boolean>}
   */
  isSupportedByCurrentProvider: computed('sessionProviderId', 'providers.[]', function () {
    const {
      providers,
      sessionProviderId,
    } = this.getProperties('sessionProviderId', 'providers');
    if (isArray(providers) && sessionProviderId != null) {
      return _.includes(providers.map(p => get(p, 'id')), sessionProviderId);
    } else {
      return null;
    }
  }),
});
