/**
 * Table listing transfers with details show on click.
 * 
 * Works in two modes: desktop (models-table) or mobile (one-collapsible-list)
 *
 * @module components/transfers/live-stats-table
 * @author Michal Borzecki, Jakub Liput
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import TransferTableRecord from 'op-worker-gui/utils/transfer-table-record';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import _ from 'lodash';

const {
  Component,
  computed,
  observer,
  inject: {
    service,
  },
  run: {
    scheduleOnce,
  },
  get,
  set,
  setProperties,
  A,
  Object: EmberObject,
} = Ember;

const COMMON_I18N_PREFIX = 'components.transfers.';
const I18N_PREFIX = COMMON_I18N_PREFIX + 'liveTableStats.';

const tableExcludedColumns = {
  scheduled: ['startedAt', 'finishedAt', 'totalBytes', 'totalFiles'],
  current: ['scheduledAt', 'finishedAt'],
  completed: ['scheduledAt'],
};

export default Component.extend({
  classNames: ['transfers-live-stats-table', 'transfers-table'],
  classNameBindings: ['transferType'],

  i18n: service(),
  notify: service(),

  /**
   * @virtual 
   * @type {Array<Transfer>}
   */
  transfers: undefined,

  /**
   *
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,
  
  /**
   * @virtual 
   * @type {Function}
   */
  notifyLoaded: () => {},
  
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
   * @virtual
   * @type {Function}
   * External implementation of rerunTransfer that should actually invoke
   * a procedure
   * @returns {Promise<undefined|any>} promise should resolve when rerunning has
   *    started successfully
   */
  rerunTransfer: undefined,
  
  /**
   * @virtual 
   * @type {Function}
   */
  notifyTransferListChanged: () => {},
  
  /**
   * @virtual 
   * @type {Function} (boolean) => undefined
   */
  stickyTableChanged: () => {},
  
  /**
   * Type of transfers. May be `scheduled`, `current` or `completed`
   * @public
   * @type {string}
   */
  transferType: 'current',

  /**
   * Which transfers should be presented as selected on table render
   * @public
   * @type {Array<string>|undefined} array of transfer ids
   */
  selectedTransferIds: undefined,
      
  /**
   * @type {EmberObject}
   */
  firstRowSpace: undefined,
  
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
   * @type {number}
   */
  _stickyTableHeaderOffset: 0,
  
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
    const messageId = `noTransfers.${this.get('transferType')}`;
    return EmberObject.create({
      noDataToShow: this.get('i18n').t(COMMON_I18N_PREFIX + messageId),
    });
  }),
  
  /**
   * @type {Array<Object>}
   */
  _tableDataCache: null,
  
  _tableData: computed(
    'transfers.[]',
    'transfers.{startIndex,endIndex}',
    'providers',
    'providersColors',
    'selectedTransferIds.[]',
    '_rowActions',
    function () {
      const {
        transfers,
        providers,
        providersColors,
        selectedTransferIds,
        _tableDataCache,
        firstRowSpace,
        movedTransfers,
        updaterId,
        _rowActions,
      } = this.getProperties(
        'transfers',
        'providers',
        'providersColors',
        'selectedTransferIds',
        '_tableDataCache',
        'firstRowSpace',
        'movedTransfers',
        'updaterId',
        '_rowActions'
      );
      
      if (transfers && providers) {
        let arrayChanged = false;
        const cachedTransfers = _tableDataCache.mapBy('transfer');
        const initialCacheLength = get(_tableDataCache, 'length');
        _.pullAllWith(
          _tableDataCache,
          _.difference(
            cachedTransfers,
            transfers.toArray()
          ),
          (cached, transfer) => {
            const ct = get(cached, 'transfer');
            return cached && transfer && ct === transfer;
          }
        );
        if (get(_tableDataCache, 'length') !== initialCacheLength) {
          arrayChanged = true;
        }
        
        transfers.forEach(transfer => {
          if (!_.includes(cachedTransfers, transfer)) {
            const transferId = get(transfer, 'id');
            // force load record
            if (!get(transfer, 'isLoaded') && !get(transfer, 'isLoading')) {
              transfer.store.findRecord('transfer', transferId);
            } else if (movedTransfers.has(transferId)) {
              transfer.reload().then(transfer => {
                transfer.belongsTo('currentStat').reload();
              });
            }
            _tableDataCache.push(TransferTableRecord.create({
              selectedTransferIds,
              transfer,
              transfers,
              providers,
              providersColors,
              updaterId,
              actions: _rowActions,
            }));
            arrayChanged = true;
          }
        });
        _tableDataCache.sort((a, b) => get(a, 'listIndex') - get(b, 'listIndex'));
        const topRecord = _.minBy(_tableDataCache.slice(1), tc => get(tc, 'listIndex'));
        set(
          firstRowSpace,
          'firstRowListIndex',
          topRecord && get(topRecord, 'listIndex')
        );
        if (arrayChanged) {
          _tableDataCache.arrayContentDidChange();
        }
      }
      
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
    const excludedColumns = tableExcludedColumns[transferType];
        
    // field `id` is custom and is used only to check which column should be 
    // filtered out for active/completed table version
    const allColumns = [
      {
        id: 'listIndex',
        propertyName: 'listIndex',
        isHidden: true,
        sortDirection: 'asc',
        sortPrecedence: 1,
      },
      {
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
        id: 'scheduledAt',
        propertyName: 'scheduledAtReadable',
        title: i18n.t(I18N_PREFIX + 'scheduledAt'),
      }, {
        id: 'startedAt',
        propertyName: 'startedAtReadable',
        title: i18n.t(I18N_PREFIX + 'startedAt'),
      }, {
        id: 'finishedAt',
        propertyName: 'finishedAtReadable',
        title: i18n.t(I18N_PREFIX + 'finishedAt'),
      }, {
        id: 'totalBytes',
        propertyName: 'totalBytesReadable',
        title: i18n.t(I18N_PREFIX + 'totalBytes'),
        component: 'transfers/live-stats-table/cell-errorable',
      }, {
        id: 'totalFiles',
        propertyName: 'totalFiles',
        title: i18n.t(I18N_PREFIX + 'totalFiles'),
        component: 'transfers/live-stats-table/cell-total-files',
      },
      {
        id: 'type',
        propertyName: 'type',
        className: 'col-icon',
        title: i18n.t(I18N_PREFIX + 'type'),
        component: _mobileMode ? undefined : 'transfers/live-stats-table/cell-type',
      },
      {
        id: 'status',
        propertyName: 'status',
        className: 'col-icon',
        title: i18n.t(I18N_PREFIX + 'status'),
        component: 'transfers/live-stats-table/cell-status',
      },
    ];
    if (!_mobileMode) {
      allColumns.push({
        id: 'actions',
        component: 'transfers/live-stats-table/cell-actions',
        className: 'transfer-actions-cell',
      });
    }
    allColumns.forEach(column => column.disableSorting = true);
    return _.differenceWith(allColumns, excludedColumns, (col, eid) => col.id === eid);
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
  
  _hasExpandableRows: computed('transferType', function getExpandableRows() {
    const transferType = this.get('transferType');
    return transferType !== 'scheduled';
  }),

  /**
   * Internal cancel of transfer which knows which row (table record) invoked
   * the procedure, so it can modify row (record) state.
   * @param {object} record instance of model-table record for which the transfer
   *    has been canceled
   */
  _cancelTransfer: computed('cancelTransfer', function () {
    const cancelTransfer = this.get('cancelTransfer');
    return cancelTransfer ? (record) => {
      const {
        notify,
        i18n,
      } = this.getProperties('notify', 'i18n');
      setProperties(record, {
        actionMessage: undefined,
        actionMessageType: undefined,
      });
      set(record, 'transfer.isCancelling', true);
      cancelTransfer(get(record, 'transfer.id'))
        .catch(error => {
          notify.error(i18n.t(I18N_PREFIX + 'cancelFailure'));
          safeExec(record, () => setProperties(record, {
            actionMessage: i18n.t(I18N_PREFIX + 'cancelFailure'),
            actionMessageType: 'failure',
          }));
          safeExec(get(record, 'transfer'), 'set', 'isCancelling', false);
          throw error;
        })
        .then(() => {
          return record.transfer.reload();
        });
    } : undefined;
  }),
  
  /**
   * Internal rerun of transfer which knows which row (table record) invoked
   * the procedure, so it can modify row (record) state.
   * @param {object} record instance of model-table record for which the transfer
   *    has been rerun
   */
  _rerunTransfer: computed('rerunTransfer', function () {
    const rerunTransfer = this.get('rerunTransfer');
    return rerunTransfer ? (record) => {
      const {
        notify,
        i18n,
      } = this.getProperties('notify', 'i18n');
      setProperties(record, {
        actionMessage: i18n.t(I18N_PREFIX + 'rerunStarting'),
        actionMessageType: 'warning',
        isRerunning: true,
      });
      rerunTransfer(get(record, 'transfer.id'))
        .catch(error => {
          setProperties(record, {
            actionMessage: i18n.t(I18N_PREFIX + 'rerunFailure'),
            actionMessageType: 'failure',
          });
          notify.error(i18n.t(I18N_PREFIX + 'rerunFailure'));
          throw error;
        })
        .then(() => {
          setProperties(record, {
            actionMessage: i18n.t(I18N_PREFIX + 'rerunSuccess'),
            actionMessageType: 'success',
          });
          notify.success(i18n.t(I18N_PREFIX + 'rerunSuccess'));
          return record.transfer.reload();
        })
        .finally(() => {
          record.set('isRerunning', false);
        });
    } : undefined;
  }),

  /**
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _rowActions: computed('_cancelTransfer', '_rerunTransfer', function () {
    const {
      _cancelTransfer,
      _rerunTransfer,
    } = this.getProperties('_cancelTransfer', '_rerunTransfer');
    const actions = [];
    if (_cancelTransfer) {
      actions.push({
        id: 'cancelTransfer',
        action: _cancelTransfer,
      });
    }
    if (_rerunTransfer) {
      actions.push({
        id: 'rerunTransfer',
        action: _rerunTransfer,
      });
    }
    return actions;
  }),
    
  transferListChanged: observer('_tableDataCache.[]', function() {
    scheduleOnce(
      'afterRender',
      this,
      'notifyTransferListChanged',
      this.get('transferType')
    );

  }),
  
  init() {
    this._super(...arguments);

    let {
      _resizeEventHandler,
      _window,
    } = this.getProperties('_resizeEventHandler', '_window');
    
    const firstRowSpace = this.set(
      'firstRowSpace',
      EmberObject.create({ firstRowSpace: true, listIndex: -1 })
    );
    
    this.set(
      '_tableDataCache', 
      A([firstRowSpace])
    );
    
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
    
  actions: {
    stickyHeaderChanged(state) {
      this.get('stickyTableChanged')(state);
    },
  },
});
