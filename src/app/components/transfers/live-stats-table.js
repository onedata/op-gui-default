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
   * Type of transfers. May be `active` or `completed`
   * @type {string}
   */
  transferType: 'active',

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
    'transfers.@each.{tableDataIsLoaded,status,finishTime,fileType,transferredBytes,transferredFiles}',
    'providers',
    'providersColors',
    function () {
      const _tableDataCache = this.get('_tableDataCache');
      const {
        transfers,
        providers,
        providersColors,
        i18n,
      } = this.getProperties('transfers', 'providers', 'providersColors', 'i18n');
      
      if (transfers && providers) {
        const newTableData = transfers.map((transfer) => transferTableData(
          this._getIndexForTransfer(transfer),
          transfer,
          providers,
          providersColors,
          i18n
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
  _tableColumns: computed('transferType', '_mobileMode', function () {
    const {
      i18n,
      transferType,
      _mobileMode,
    } = this.getProperties('i18n', 'transferType', '_mobileMode');
    const onlyCompletedColumns = ['finishedAt'];

    // field `id` is custom and is used only to check which column should be 
    // filtered out for active/completed table version
    const allColumns = [{
      id: 'path',
      propertyName: 'path',
      title: i18n.t(I18N_PREFIX + 'path'),
      component: _mobileMode ?
        undefined : 'transfers/live-stats-table/cell-file-name',
    }, {
      id: 'userName',
      propertyName: 'userName',
      title: i18n.t(I18N_PREFIX + 'userName'),
      component: _mobileMode ?
        undefined : 'transfers/live-stats-table/cell-truncated',
    }, {
      id: 'destination',
      propertyName: 'destination',
      title: i18n.t(I18N_PREFIX + 'destination'),
      component: _mobileMode ?
        undefined : 'transfers/live-stats-table/cell-truncated',
    }, {
      id: 'startedAt',
      propertyName: 'startedAtReadable',
      sortedBy: 'startedAtComparable',
      sortPrecedence: 1,
      sortDirection: 'desc',
      title: i18n.t(I18N_PREFIX + 'startedAt'),
    }, {
      id: 'finishedAt',
      propertyName: 'finishedAtReadable',
      sortedBy: 'finishedAtComparable',
      title: i18n.t(I18N_PREFIX + 'finishedAt'),
    }, {
      id: 'totalBytes',
      propertyName: 'totalBytesReadable',
      sortedBy: 'totalBytes',
      title: i18n.t(I18N_PREFIX + 'totalBytes'),
    }, {
      id: 'totalFiles',
      propertyName: 'totalFiles',
      title: i18n.t(I18N_PREFIX + 'totalFiles'),
    }, {
      id: 'status',
      propertyName: 'status',
      title: i18n.t(I18N_PREFIX + 'status'),
      component: 'transfers/live-stats-table/cell-status',
    }];
    if (transferType === 'active') {
      return allColumns.filter((column) => 
        onlyCompletedColumns.indexOf(column.id) === -1
      );
    } else {
      return allColumns;
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
 */
function transferTableData(transferIndex, transfer, providers, providersColors, i18n) {
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
    'tableDataIsLoaded'
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
  });
}
