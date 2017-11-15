import Ember from 'ember';
import moment from 'moment';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';
import mergeNewItems from 'ember-cli-onedata-common/utils/push-new-items';

const {
  computed,
  inject: {
    service,
  },
  get,
  A,
  Object: EmberObject,
} = Ember;

const START_END_TIME_FORMAT = 'D MMM YYYY H:mm:ss';
const I18N_PREFIX = 'components.transfers.liveTableStats.';

export default Ember.Component.extend({
  classNames: ['transfers-live-stats-table', 'transfers-table'],

  i18n: service(),

  // FIXME: provider transfer data
  /**
   * @virtual 
   * @type {Array<Transfer>}
   */
  transfers: undefined,

  /**
   * Type of transfers. May be `active` or `completed`
   * @type {string}
   */
  transferType: 'active',

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
  _tableCustomIcons: Ember.Object.create({
    'sort-asc': 'oneicon oneicon-arrow-up',
    'sort-desc': 'oneicon oneicon-arrow-down',
  }),

  /**
   * Custom classes for ember-models-table addon.
   * @type {Ember.Object}
   */
  _tableCustomClasses: computed(function () {
    return Ember.Object.create({
      table: 'table',
    });
  }),

  /**
   * Custom messages for ember-models-table addon.
   * @type {Ember.Object}
   */
  _tableCustomMessages: computed('noDataToShowMessage', function () {
    return Ember.Object.create({
      noDataToShow: this.get('i18n').t(I18N_PREFIX + 'noTransfersToShow'),
    });
  }),
  
  _tableDataCache: null,
  
  // FIXME: make objects with required async data
  // FIXME: handle loading of data (async - table shoul present loading state)
  // FIXME: this should be a static reference to array to prevent re-rendering
  /**
   * Transfers converted to format used by table.
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _tableData: computed('transfers.@each.tableDataIsLoaded', function () {
    let _tableDataCache = this.get('_tableDataCache');
    const transfers = this.get('transfers') || A([]);
    const newTableData = transfers.map(transferTableData);
    mergeNewItems(
      _tableDataCache,
      newTableData,
      (a, b) => get(a, 'transferId') === get(b, 'transferId'),
      false
    );
    
    return this.set('_tableDataCache', _tableDataCache);
  }),

  /**
   * Table columns definition.
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _tableColumns: computed('transferType', function () {
    const {
      i18n,
      transferType,
    } = this.getProperties('i18n', 'transferType');
    const onlyCompletedColumns = ['finishedAt', 'status'];

    // field `id` is custom and is used only to check which column should be 
    // filtered out for active/completed table version
    const allColumns = [{
      id: 'path',
      propertyName: 'path',
      title: i18n.t(I18N_PREFIX + 'path'),
      component: 'transfers/live-stats-table/cell-file-name',
    }, {
      id: 'userName',
      propertyName: 'userName',
      title: i18n.t(I18N_PREFIX + 'userName'),
      component: 'transfers/live-stats-table/cell-truncated',
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
      this.set('_mobileMode', this.get('_window.innerWidth') < 1051);
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
});

// TODO: optimize using destructurize and getProperties
function transferTableData(transfer) {
  const transferId = get(transfer, 'id');
  const path = get(transfer, 'path');
  const fileType = get(transfer, 'fileType');
  const startTimestamp = get(transfer, 'startTime');
  const startMoment = moment.unix(startTimestamp);
  const finishTimestamp = get(transfer, 'finishTime');
  const finishMoment = moment.unix(finishTimestamp);
  // FIXME: async properties - add loading states?
  const transferredBytes = get(transfer, 'currentStat.transferredBytes');
  const transferredFiles = get(transfer, 'currentStat.transferredFiles');
  const userName = get(transfer, 'userName');
  const startedAtReadable = startMoment && startMoment.format(START_END_TIME_FORMAT);
  const finishedAtReadable = finishMoment && finishMoment.format(START_END_TIME_FORMAT);
  const totalBytesReadable = bytesToString(transferredBytes);
  const status = get(transfer, 'status');
  const isLoading = get(transfer, 'tableDataIsLoaded') === false;
  
  return EmberObject.create({
    // FIXME: user name is async, so it should be in loading state
    transfer,
    transferId,
    path,
    fileType,
    userName,
    startedAtComparable: startTimestamp,
    startedAtReadable,
    finishedAtComparable: finishTimestamp,
    finishedAtReadable,
    totalBytes: transferredBytes,
    totalBytesReadable,
    totalFiles: transferredFiles,
    status,
    isLoading, // FIXME: true if loading async fields
  });
}
