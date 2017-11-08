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
  /**
   * Transfers converted to format used by table.
   * @type {Ember.ComputedProperty<Array<Object>>}
   */
  _tableData: computed('transfers.[]', function () {
    const transfers = this.get('transfers') || [];
    // FIXME: debug
    const _tableData = transfers.map(transfer => {
      const startMoment = moment(transfer.startTime);
      return {
        // TODO: get user name
        userName: transfer.get('systemUser.name'),
        startedAtComparable: transfer.startTime,
        startedAtReadable: startMoment.format(START_TIME_FORMAT),
        // FIXME: total bytes should be get from currentStat.transferredBytes
        totalBytes: get(transfer, 'currentStat.transferredBytes'),
        totalBytesReadable: bytesToString(get(transfer, 'currentStat.transferredBytes')),
        // totalFiles: transfer.totalFiles,
        // FIXME: api changed, no global stats object
        // stats: get(transfer, stats)
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
