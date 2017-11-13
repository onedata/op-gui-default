import Ember from 'ember';
import moment from 'moment';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';

const {
  computed,
  inject: {
    service,
  },
  get,
} = Ember;

const START_TIME_FORMAT = 'D MMM YYYY H:mm:ss';
const I18N_PREFIX = 'components.transfers.liveTableStats.';

export default Ember.Component.extend({
  classNames: ['transfers-live-stats-table'],

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
  
  // FIXME: make objects with required async data
  // FIXME: handle loading of data (async - table shoul present loading state)
  // FIXME: this should be a static reference to array to prevent re-rendering
  /**
   * Transfers converted to format used by table.
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _tableData: computed('transfers.@each.isLoaded', function () {
    const transfers = this.get('transfers') || [];
    // FIXME: debug
    const _tableData = transfers
      .filter(t => get(t, 'isLoaded'))
      .map(transfer => {
        
        // FIXME: test forcing stats to be fetched
        window._mistat = get(transfer, 'minuteStat');
        window._hstat = get(transfer, 'hourStat');
        window._dstat = get(transfer, 'dayStat');
        window._mostat = get(transfer, 'monthStat');
        
        const path = get(transfer, 'path');
        const fileType = get(transfer, 'fileType');
        const startTimestamp = get(transfer, 'startTime');
        const startMoment = moment.unix(startTimestamp);
        // FIXME: async properties - add loading states?
        const transferredBytes = get(transfer, 'currentStat.transferredBytes');
        const transferredFiles = get(transfer, 'currentStat.transferredFiles');
        const userName = get(transfer, 'systemUser.name');
        return {
          // FIXME: user name is async, so it should be in loading state
          path,
          fileType,
          userName,
          startedAtComparable: startTimestamp,
          startedAtReadable: startMoment.format(START_TIME_FORMAT),
          totalBytes: transferredBytes,
          totalBytesReadable: bytesToString(transferredBytes),
          totalFiles: transferredFiles,
          isLoadingCurrentStat: false, // FIXME: true if loading async currentStat
        };
      });
    return _tableData;
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
