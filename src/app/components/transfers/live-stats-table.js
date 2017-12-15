/**
 * Table listing transfers with details show on click.
 * 
 * Works in two modes: desktop (models-table) or mobile (one-collapsible-list)
 *
 * @module components/transfers/live-stats-table
 * @author Michal Borzecki, Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import moment from 'moment';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import mutateArray from 'ember-cli-onedata-common/utils/mutate-array';
import _ from 'lodash';

const {
  Component,
  computed,
  inject: {
    service,
  },
  get,
  set,
  getProperties,
  A,
  Object: EmberObject,
} = Ember;

const START_END_TIME_FORMAT = 'D MMM YYYY H:mm:ss';
const COMMON_I18N_PREFIX = 'components.transfers.';
const I18N_PREFIX = COMMON_I18N_PREFIX + 'liveTableStats.';

export default Component.extend({
  classNames: ['transfers-live-stats-table', 'transfers-table'],

  i18n: service(),
  notify: service(),

  /**
   * @virtual 
   * @type {Array<Transfer>}
   */
  transfers: undefined,

  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,
  
  /**
   * @virtual 
   * @type {Function}
   */
  notifyLoaded: () => {},
  
  // TODO: using undefined, because _we want_ to see error when this is not injected
  /**
   * @virtual
   * @type {Function}
   * External implementation of cancelTransfer that should actually invoke
   * a procedure
   * @returns {Promise<undefined|any>} promise should resolve when cancelling has
   *    started successfully
   */
  cancelTransfer: undefined,
  
  /**
   * Type of transfers. May be `active` or `completed`
   * @public
   * @type {string}
   */
  transferType: 'active',

  /**
   * Which transfers should be presented as selected on table render
   * @public
   * @type {Array<string>|undefined} array of transfer ids
   */
  selectedTransferIds: undefined,
  
  /**
   * Global mapping: transferId -> integer index
   * @type {Object}
   */
  _transferIdToIndexMap: {
    nextTransferIndex: 0,
    mapping: {},
  },

  /**
   * If true, component is rendered in mobile mode.
   * @type {boolean}
   */
  _mobileMode: false,

  /**
   * Window object (for testing purposes).
   * @type {Window}
   */
  _window: window,

  /**
   * Custom icons for ember-models-table addon.
   * @type {Ember.Object}
   */
  _tableCustomIcons: EmberObject.create({
    'sort-asc': 'oneicon oneicon-arrow-up',
    'sort-desc': 'oneicon oneicon-arrow-down',
  }),

  /**
   * Custom classes for ember-models-table addon.
   * @type {Ember.Object}
   */
  _tableCustomClasses: computed(function () {
    return EmberObject.create({
      table: 'table',
    });
  }),

  /**
   * Custom messages for ember-models-table addon.
   * @type {Ember.Object}
   */
  _tableCustomMessages: computed('transferType', function () {
    const messageId = (this.get('transferType') === 'active') ?
      'noActiveTransfers' : 'noCompletedTransfers';
    return EmberObject.create({
      noDataToShow: this.get('i18n').t(COMMON_I18N_PREFIX + messageId),
    });
  }),
  
  /**
   * @type {Array<Object>}
   */
  _tableDataCache: null,
  
  // TODO: this causes n*n invoking this computed property, but transferTableData
  // function is invoked only few times, maybe to refactor, but it's a hard piece of code...
  /**
   * Transfers converted to format used by table.
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _tableData: computed(
    'transfers.@each.{tableDataIsLoaded,status,finishTime,fileType,transferredBytes,transferredFiles,currentStatError}',
    'providers',
    'providersColors',
    'selectedTransferIds.[]',
    function () {
      const _tableDataCache = this.get('_tableDataCache');
      const {
        transfers,
        providers,
        providersColors,
        i18n,
        selectedTransferIds,
      } = this.getProperties('transfers', 'providers', 'providersColors', 'i18n', 'selectedTransferIds');
      
      if (transfers && providers) {
        const newTableData = transfers.map((transfer) => transferTableData(
          this._getIndexForTransfer(transfer),
          transfer,
          providers,
          providersColors,
          i18n,
          selectedTransferIds,
          this.get('_cancelTransfer')
        ));
        mutateArray(
          _tableDataCache,
          newTableData,
          (a, b) => get(a, 'transferId') === get(b, 'transferId'),
          false
        );
      }
      
      _tableDataCache.sort((a, b) => 
        get(b, 'startedAtComparable') - get(a, 'startedAtComparable')
      );

      return _tableDataCache;
    }
  ),

  /**
   * Table columns definition.
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _tableColumns: computed('transferType', '_mobileMode', 'sortBy', function () {
    const {
      i18n,
      transferType,
      _mobileMode,
      sortBy,
    } = this.getProperties('i18n', 'transferType', '_mobileMode', 'sortBy');
    const onlyCompletedColumns = ['finishedAt'];
    const onlyActiveColumns = ['actions'];
    const isTransferActive = (transferType === 'active');
        
    // field `id` is custom and is used only to check which column should be 
    // filtered out for active/completed table version
    const allColumns = [{
      id: 'path',
      propertyName: 'path',
      title: i18n.t(I18N_PREFIX + 'path'),
      component: _mobileMode ?
        undefined : 'transfers/live-stats-table/cell-file-name',
      sortPrecedence: sortBy === 'path' ? 2 : undefined,
      sortDirection: sortBy === 'path' ? 'desc' : undefined,
    }, {
      id: 'userName',
      propertyName: 'userName',
      title: i18n.t(I18N_PREFIX + 'userName'),
      component: _mobileMode ?
        undefined : 'transfers/live-stats-table/cell-truncated',
      sortPrecedence: sortBy === 'userName' ? 2 : undefined,
      sortDirection: sortBy === 'userName' ? 'desc' : undefined,
    }, {
      id: 'destination',
      propertyName: 'destination',
      title: i18n.t(I18N_PREFIX + 'destination'),
      component: _mobileMode ?
        undefined : 'transfers/live-stats-table/cell-truncated',
      sortPrecedence: sortBy === 'destination' ? 2 : undefined,
      sortDirection: sortBy === 'destination' ? 'desc' : undefined,
    }, {
      id: 'startedAt',
      propertyName: 'startedAtReadable',
      sortedBy: 'startedAtComparable',
      sortPrecedence: isTransferActive ? 1 : undefined,
      sortDirection: isTransferActive ? 'desc' : undefined,
      title: i18n.t(I18N_PREFIX + 'startedAt'),
    }, {
      id: 'finishedAt',
      propertyName: 'finishedAtReadable',
      sortedBy: 'finishedAtComparable',
      sortPrecedence: isTransferActive ? undefined : 1,
      sortDirection: isTransferActive ? undefined : 'desc',
      title: i18n.t(I18N_PREFIX + 'finishedAt'),
    }, {
      id: 'totalBytes',
      propertyName: 'totalBytesReadable',
      sortedBy: 'totalBytes',
      title: i18n.t(I18N_PREFIX + 'totalBytes'),
      component: 'transfers/live-stats-table/cell-errorable',
      sortPrecedence: sortBy === 'totalBytes' ? 2 : undefined,
      sortDirection: sortBy === 'totalBytes' ? 'desc' : undefined,
    }, {
      id: 'totalFiles',
      propertyName: 'totalFiles',
      title: i18n.t(I18N_PREFIX + 'totalFiles'),
      component: 'transfers/live-stats-table/cell-errorable',
      sortPrecedence: sortBy === 'totalFiles' ? 2 : undefined,
      sortDirection: sortBy === 'totalFiles' ? 'desc' : undefined,
    }, {
      id: 'status',
      propertyName: 'status',
      title: i18n.t(I18N_PREFIX + 'status'),
      component: 'transfers/live-stats-table/cell-status',
      sortPrecedence: sortBy === 'status' ? 2 : undefined,
      sortDirection: sortBy === 'status' ? 'desc' : undefined,
    }, {
      id: 'actions',
      component: 'transfers/live-stats-table/cell-actions',
      className: 'transfer-actions-cell',
      notClickable: true,
      headerClassName: 'transfer-actions-cell',
      disableSorting: true,
      disableMobile: true,
    }];
    if (isTransferActive) {
      return allColumns.filter((column) => 
        onlyCompletedColumns.indexOf(column.id) === -1
      );
    } else {
      return allColumns.filter((column) => 
        onlyActiveColumns.indexOf(column.id) === -1
      );
    }
  }),

  /**
   * Window resize event handler.
   * @type {Ember.ComputedProperty<Function>}
   */
  _resizeEventHandler: computed(function () {
    return () => {
      this.set('_mobileMode', this.get('_window.innerWidth') < 1200);
    };
  }),
  
  init() {
    this._super(...arguments);

    let {
      _resizeEventHandler,
      _window,
    } = this.getProperties('_resizeEventHandler', '_window');
    
    this.set('_tableDataCache', A());

    _resizeEventHandler();
    _window.addEventListener('resize', _resizeEventHandler);
  },

  willDestroyElement() {
    try {
      let {
        _resizeEventHandler,
        _window,
      } = this.getProperties('_resizeEventHandler', '_window');
      _window.removeEventListener('resize', _resizeEventHandler);
    } finally {
      this._super(...arguments);
    }
  },

  /**
   * Internal cancel of transfer which knows which row (table record) invoked
   * the procedure, so it can modify row (record) state.
   * @param {object} record instance of model-table record for which the transfer
   *    has been canceled
   */
  _cancelTransfer: computed('cancelTransfer', function () {
    const cancelTransfer = this.get('cancelTransfer');
    return (record) => {
      const {
        notify,
        i18n,
      } = this.getProperties('notify', 'i18n');
      set(record, 'transfer.isCancelling', true);
      cancelTransfer(record.transferId)
        .catch(error => {
          notify.error(i18n.t(I18N_PREFIX + 'cancelFailure'));
          throw error;
        })
        .then(() => {
          return record.transfer.reload();
        })
        .finally(() => {
          set(record, 'transfer.isCancelling', false);
        });
    };
  }),
  
  /**
   * Returns unique number id for transfer
   * @param {Transfer} transfer
   * @returns {number}
   */
  _getIndexForTransfer(transfer) {
    const _transferIdToIndexMap = this.get('_transferIdToIndexMap');
    const transferId = get(transfer, 'id');
    if (_transferIdToIndexMap.mapping[transferId] === undefined) {
      _transferIdToIndexMap.mapping[transferId] = _transferIdToIndexMap.nextTransferIndex++;
    }
    return _transferIdToIndexMap.mapping[transferId];
  }
});

/**
 * Create data object for live stats table row with transfer data
 * @param {Transfer} transfer 
 * @param {Array<Provider>} providers 
 * @param {Object} providersColors 
 * @param {Ember.Service} i18n i18n service instance (`t` method)
 * @param {Array<string>|undefined} selectedTransferIds
 * @param {Function} cancelTransfer a function to invoke to cancel transfer
 *    `cancelTransfer(transferId)`
 */
function transferTableData(transferIndex, transfer, providers, providersColors, i18n, selectedTransferIds, cancelTransfer) {
  // searching for destination
  let destination = i18n.t(I18N_PREFIX + 'destinationUnknown');
  const destProvider = _.find(providers, (provider) => 
    get(provider, 'id') === get(transfer, 'destination')
  );
  if (destProvider) {
    destination = get(destProvider, 'name');
  }
  
  const {
    id: transferId,
    path,
    fileType,
    startTime: startTimestamp,
    finishTime: finishTimestamp,
    currentStat,
    userName,
    status,
    tableDataIsLoaded,
    currentStatError,
  } = getProperties(
    transfer,
    'id',
    'path',
    'fileType',
    'startTime',
    'finishTime',
    'currentStat',
    'userName',
    'status',
    'tableDataIsLoaded',
    'currentStatError'
  );
  const startMoment = moment.unix(startTimestamp);
  const finishMoment = moment.unix(finishTimestamp);
  const {
    transferredBytes,
    transferredFiles
  } = getProperties(currentStat, 'transferredBytes', 'transferredFiles');
  
  const startedAtReadable = startMoment && startMoment.format(START_END_TIME_FORMAT);
  const finishedAtReadable = finishMoment && finishMoment.format(START_END_TIME_FORMAT);
  const totalBytesReadable = bytesToString(transferredBytes);
  const isLoading = (tableDataIsLoaded === false);
  const initSelect = _.includes(selectedTransferIds, transferId);
  
  const actions = [
    {
      id: 'cancelTransfer',
      action: cancelTransfer,
    },
  ];
  return EmberObject.create({
    transfer,
    transferIndex,
    transferId,
    providers,
    providersColors,
    path,
    fileType,
    destination,
    userName,
    startedAtComparable: startTimestamp,
    startedAtReadable,
    finishedAtComparable: finishTimestamp,
    finishedAtReadable,
    totalBytes: transferredBytes,
    totalBytesReadable,
    totalFiles: transferredFiles,
    status,
    isLoading,
    currentStatError,
    initSelect,
    actions,
  });
}
