/**
 * A container for components for showing transfers for single space.
 * 
 * It takes a space and prepares transfer data needed for rendering other components.
 * 
 * @module components/transfers/data-container
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

const {
  Component,
  computed,
  A,
  get,
  inject: { service },
  observer,
  isEmpty,
  run,
  RSVP: {
    Promise,
  },
} = Ember;

import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import providerTransferConnections from 'op-worker-gui/utils/transfers/provider-transfer-connections';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import generateColors from 'op-worker-gui/utils/generate-colors';
import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';

const RE_TRANSFER_ROW_ID = /transfer-row-(.*)/;

export default Component.extend({
  classNames: ['transfers-data-container'],
  
  session: service(),
  store: service(),
    
  /**
   * @virtual
   * @type {Space}
   */
  space: undefined,
  
  /**
   * @virtual
   * @type {Array<string>|undefined}
   */
  selectedTransferIds: undefined,

  /**
   * @public
   * Manually enable/disable updater (eg. for testing)
   * @type {boolean}
   */
  transfersUpdaterEnabled: true,
  
  /**
   * Updates transfers data needed by most of visible components.
   * Initialized in `init`.
   * @type {SpaceTransfersUpdater}
   */
  transfersUpdater: undefined,
  
  providerId: computed.reads('session.sessionDetails.providerId'),
  
  _transfersUpdaterEnabled: computed.readOnly('transfersUpdaterEnabled'),
  
  //#region Space properties aliases
  
  currentTransferList: computed.reads('space.currentTransferList'),
  completedTransferList: computed.reads('space.completedTransferList'),
  onTheFlyTransferList: computed.reads('space.onTheFlyTransferList'),
  providerList: computed.reads('space.providerList'),
  providersMap: computed.reads('space.transferLinkState.activeLinks'),
  
  //#endregion
  
  /**
   * Collection of Transfer model for current
   * (active, invalidating or scheduled) transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  currentTransfers: computed.reads('currentTransferList.list.content'),
  
  /**
   * Collection of Transfer model for completed transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  completedTransfers: computed.reads('completedTransferList.list.content'),

  /**
   * Collection of Transfer model for on the fly transfers
   * @type {Ember.ComputedProperty<Ember.Array<Transfer>>}
   */
  onTheFlyTransfers: computed.reads('onTheFlyTransferList.list.content'),

  /**
   * List of providers that support this space
   * @type {Ember.ComputedProperty<Ember.Array<Provider>>}
   */
  providers: computed.reads('providerList.queryList.content'),
  
  //#region Loading and error states of yielded values
  
  providersLoaded: computed.reads('providerList.queryList.isSettled'),
  providersError: computed.reads('providerList.queryList.reason'),

  currentTransfersLoaded: computed(
    'currentTransferList.isLoaded',
    'currentTransfers.isLoaded',
    function getCurrentTransfersLoaded() {
      return this.get('currentTransferList.isLoaded') === true &&
        this.get('currentTransfers.isLoaded') === true;
    }
  ),
  
  completedTransfersLoaded: computed(
    'completedTransferList.isLoaded',
    'completedTransfers.isLoaded',
    function getCompletedTransfersLoaded() {
      return this.get('completedTransferList.isLoaded') === true &&
        this.get('completedTransfers.isLoaded') === true;
    }
  ),

  onTheFlyTransfersLoaded: computed(
    'onTheFlyTransferList.isLoaded',
    'onTheFlyTransfers.isLoaded',
    'onTheFlyTransfers.@each.isLoading',
    function getCurrentTransfersLoaded() {
      return this.get('onTheFlyTransferList.isLoaded') === true &&
        this.get('onTheFlyTransfers.isLoaded') === true &&
        this.get('onTheFlyTransfers').every(transfer => !transfer.get('isLoading'));
    }
  ),

  onTheFlyProviders: computed('onTheFlyTransfersLoaded', 'providersLoaded', function () {
    const {
      space,
      providersLoaded,
      providers,
      onTheFlyTransfers,
      onTheFlyTransfersLoaded,
      store,
    } = this.getProperties(
      'space',
      'providersLoaded',
      'providers',
      'onTheFlyTransfers',
      'onTheFlyTransfersLoaded',
      'store'
    );
    let promise;
    if (!providersLoaded || !onTheFlyTransfersLoaded) {
      promise = Promise.resolve(A());
    } else {
      const providersIds =
        onTheFlyTransfers.map(transfer => transfer.get('destination'));
      const providersIdsToLoad =
        _.difference(providersIds, providers.map(p => get(p, 'id')));
      if (providersIdsToLoad.length) {
        promise = Promise.all(providersIdsToLoad.map(providerId =>
          store.queryRecord('system-provider', {
            id: providerId,
            context: {
              od_space: get(space, 'id'),
            },
          })
        )).then((loaded) => A(loaded.concat(providers)));
      } else {
        promise = Promise.resolve(A(providers));
      }
    }
    return PromiseArray.create({
      promise,
    });
  }),

  onTheFlyTransfersProvidersLoaded: computed.and(
    'onTheFlyTransfersLoaded',
    'onTheFlyProviders.isFulfilled'
  ),
  
  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  allTablesLoaded: computed(
    'providersLoaded',
    'currentTransfersLoaded',
    'completedTransfersLoaded',
    function () {
      return this.get('providersLoaded') &&
        this.get('currentTransfersLoaded'),
        this.get('completedTransfersLoaded');
    }
  ),
  
  //#endregion
  
  /**
   * If true, this instance of data container already scrolled to selected transfers
   * @type {boolean}
   */
  _scrolledToSelectedTransfers: false,

  /**
   * Cache for `providerTransferConnections`
   * @type {Ember.Array<ProviderTransferConnection>}
   */
  _ptcCache: undefined,
  
  /**
   * Collection of connection between two providers (for map display)
   * Order in connection is random; each pair can occur once.
   * See `util:transfers/provider-transfer-connections`
   * `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
   * @type {Ember.ComputedProperty<Array<ProviderTransferConnection|undefined>>}
   */
  providerTransferConnections: computed(
    'providersMap',
    '_ptcCache',
    function getProviderTransferConnections() {
      const providersMap = this.get('providersMap');
      let _ptcCache = this.get('_ptcCache');
      if (providersMap) {
        mutateArray(
          _ptcCache,
          providerTransferConnections(providersMap),
          (x, y) => x[0] === y[0] && x[1] === y[1]
        );
      }
      return _ptcCache;
    }
  ),
  
  /**
   * Creates an array of provider ids that are destination of transfers for space
   * NOTE: returns new array every recomputation
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  destinationProviderIds: computed(
    'providersMap',
    function getDestinationProviderIds() {
      const providersMap = this.get('providersMap');
      if (!isEmpty(providersMap)) {
        return _.uniq(_.flatten(_.values(providersMap)));
      }
    }
  ),
  
  /**
   * Creates an array of provider ids that are source of transfers for space
   * NOTE: returns new array every recomputation
   * @type {Ember.ComputedProperty<Array<string>>}
   */
  sourceProviderIds: computed(
    'providersMap',
    function getSourceProviderIds() {
      const providersMap = this.get('providersMap');
      if (!isEmpty(providersMap)) {
        return Object.keys(providersMap);
      }
    }
  ),

  /**
   * Global colors for each provider
   * @type {Ember.ComputedProperty<Object>}
   */
  providersColors: computed('providers.@each.id', function () {
    const providers = this.get('providers');
    if (providers) {
      const providerIds = providers.mapBy('id').sort();
      const colors = generateColors(providerIds.length + 1);
      return _.assign(
        _.zipObject(providerIds, colors),
        { 'unknown': colors[colors.length - 1] }
      );
    }
  }),
  
  /**
   * Watches updater settings dependecies and changes its settings
   */
  configureTransfersUpdater: observer(
    '_transfersUpdaterEnabled',
    'space',
    function configureTransfersUpdater() {
      const {
        _transfersUpdaterEnabled,
        space,
      } = this.getProperties(
        '_transfersUpdaterEnabled',
        'space'
      );
      this.get('transfersUpdater').setProperties({
        isEnabled: _transfersUpdaterEnabled,
        space: space,
      });
    }
  ),
    
  observeScrollToSelectedTransfers: observer('allTablesLoaded', function () { 
    if (this.get('_scrolledToSelectedTransfers') === false && this.get('allTablesLoaded')) {
      run.next(() => this._scrollToFirstSelectedTransfer());
      this.set('_scrolledToSelectedTransfers', true);
    }
  }),
  
  init() {
    this._super(...arguments);
    const {
      _transfersUpdaterEnabled,
      space,
      store,
    } = this.getProperties(
      '_transfersUpdaterEnabled',
      'space',
      'store',
      // just enable observers
      'allTablesLoaded'
    );
    
    const transfersUpdater = SpaceTransfersUpdater.create({
      store,
      isEnabled: _transfersUpdaterEnabled,
      space: space,
    });    
    this.set('transfersUpdater', transfersUpdater);
    
    this._initializeDefaultValues();
  },
  
  _scrollToFirstSelectedTransfer() {
    const selectedTransferIds = this.get('selectedTransferIds');
    
    const trs = this.$('tr.transfer-row').toArray();
    for (let i = 0; i < trs.length; i++) {
      const transferElement = trs[i];
      const tid = transferElement.id.match(RE_TRANSFER_ROW_ID)[1];
      if (_.includes(selectedTransferIds, tid)) {
        // estimate height of top toolbar + height of the table header
        // (it's better to present table header if possible)
        let navHeight;
        let thHeight;
        try {
          navHeight = parseInt(
            window.getComputedStyle($('header')[0])
              .getPropertyValue('height')
          );
          thHeight = parseInt(
            window.getComputedStyle($('.transfers-live-stats-table thead')[0])
              .getPropertyValue('height')
          );
        } catch (error) {
          console.warn(
            'component:transfers/data-container: an error occured when ' + 
            'computing scrolling offset, falling back to default'
          );
          console.warn(error);
          navHeight = 80;
          thHeight = 52;
        }
        $('#content-scroll').scrollTop($(transferElement).offset().top - (navHeight + thHeight));
        break;
      }
    }
  },
  
  _initializeDefaultValues() {
    this.set('_ptcCache', A());
  },
  
  willDestroyElement() {
    try {
      this.get('transfersUpdater').destroy();
    } finally {
      this._super(...arguments);
    }
  },
});
