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

const START_TIME_FORMAT = 'D MMM YYYY H:mm:ss';
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
    
    console.log('debug me');
    return this.set('_tableDataCache', _tableDataCache);
  }),

  /**
   * Table columns definition.
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _tableColumns: computed(function () {
    const i18n = this.get('i18n');
    return [{
      propertyName: 'path',
      title: i18n.t(I18N_PREFIX + 'path'),
      component: 'transfers/live-stats-table/cell-file-name',
    },
    {
      propertyName: 'userName',
      title: i18n.t(I18N_PREFIX + 'userName'),
    }, {
      propertyName: 'startedAtReadable',
      sortedBy: 'startedAtComparable',
      sortPrecedence: 1,
      sortDirection: 'desc',
      title: i18n.t(I18N_PREFIX + 'startedAt'),
    }, {
      propertyName: 'totalBytesReadable',
      sortedBy: 'totalBytes',
      title: i18n.t(I18N_PREFIX + 'totalBytes'),
    }, {
      propertyName: 'totalFiles',
      title: i18n.t(I18N_PREFIX + 'totalFiles'),
    }];
  }),

  /**
   * Window resize event handler.
   * @type {Ember.ComputedProperty<Function>}
   */
  _resizeEventHandler: computed(function () {
    return () => {
      this.set('_mobileMode', this.get('_window.innerWidth') < 761);
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
  // FIXME: async properties - add loading states?
  const transferredBytes = get(transfer, 'currentStat.transferredBytes');
  const transferredFiles = get(transfer, 'currentStat.transferredFiles');
  const userName = get(transfer, 'userName');
  const startedAtReadable = startMoment && startMoment.format(START_TIME_FORMAT);
  const totalBytesReadable = bytesToString(transferredBytes);
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
    totalBytes: transferredBytes,
    totalBytesReadable,
    totalFiles: transferredFiles,
    isLoading, // FIXME: true if loading async fields
  });
}
