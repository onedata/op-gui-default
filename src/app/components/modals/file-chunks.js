/**
 * Shows distribution of files among providers
 * @module components/modals/file-chunks
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';
import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import Looper from 'ember-cli-onedata-common/utils/looper';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

import _ from 'lodash';

const {
  A,
  Component,
  computed,
  observer,
  get,
  run,
  inject: { service },
  assert,
  RSVP: { Promise },
} = Ember;

const SLOW_POLLING_TIME = 10 * 1000;
const FAST_POLLING_TIME = 4 * 1000;

export default Component.extend(PromiseLoadingMixin, {
  classNames: ['file-chunks', 'file-chunks-modal'],
  
  store: service(),

  //#region External properties
  
  /**
   * @virtual
   * @type {string}
   */
  modalId: null,

  /**
   * @virtual
   * @type {function}
   */
  closedAction: () => {},

  /**
   * @virtual
   * @type {boolean}
   */
  open: false,

  /** 
   * @virtual
   * @type {File}
   */
  file: null,

  /**
   * @virtual
   * @type {Space}
   */
  space: undefined,

  /**
   * @virtual
   * @type {boolean}
   */
  currentProviderSupport: undefined,
  
  //#endregion

  //#region Internal properties
  
  chunksModalError: null,

  /**
   * @type {Array<FileDistribution>}
   */
  fileBlocks: null,

  /**
   * @type {Provider}
   */
  migrationSource: undefined,

  /**
   * @type {SpaceTransfersUpdater}
   */
  transfersUpdater: undefined,

  /**
   * If true, first time loading data of transfers after modal open.
   * Changed in open action.
   * @type {boolean}
   */
  transfersLoading: undefined,
  
  /**
   * List of provider ids for which migration was invoked, but not yet started
   * @type {Ember.Array<string>}
   */
  providerMigrationsInvoked: undefined,
  
  fileBlocksSorting: ['getProvider.name'],
  
  //#endregion
  
  /**
   * If transfers options should be disabled for current modal, returns
   * a non-empty string with main reason
   * One of: "single-provider", "proxy-provider", null
   * @type {string|null}
   */
  transferDisabledReason: computed(
    'currentProviderSupport',
    'onlySingleProviderSupport',
    function () {
      if (this.get('onlySingleProviderSupport')) {
        return 'single-provider';
      } else if (this.get('currentProviderSupport') === false) {
        return 'proxy-provider';
      } else {
        return null;
      }
    }
  ),

  /**
   * True if only one provider supports this space; undefined if cannot resolve
   * number of providers yet (eg. loading)
   * @type {Ember.ComputedProperty<boolean|undefined>} 
   */
  onlySingleProviderSupport: computed(
    'space.providerList.content.list.length',
    function getSingleProviderSupport() {
      /** @type {number|undefined} */
      const length = this.get('space.providerList.content.list.length');
      if (length != null) {
        return length === 1;
      }
    }
  ),
  
  /**
   * File distribution collection sorted (TODO: sort by provider name)
   * @type {Ember.ComputedProperty<Array<FileDistribution>|undefined>}
   */
  fileDistributionsSorted: computed(
    'providers',
    'fileBlocks.@each.provider',
    function getFileDistributionsSorted() {
      const {
        fileBlocks,
        providers,
      } = this.getProperties('fileBlocks', 'providers');
      if (fileBlocks && providers) {
        return _.sortBy(
          this.get('fileBlocks').toArray(),
          fb => get(_.find(providers, p => get(p, 'id') === get(fb, 'provider')), 'name')
        );
      }
    }
  ),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  modalSize: computed('file.isDir', function () {
    return this.get('file.isDir') ? 'md' : 'lg';
  }),

  /**
   * @type {Array<Provider>|null}
   */
  providers: computed.reads('space.providerList.queryList.content'),
  
  /**
   * True if all data for displaying table are loaded
   * @type {Ember.ComputedProperty<boolean>}
   */
  tableDataLoaded: computed(
    'file.isDir',
    'fileBlocks.@each.isLoading',
    'providers.@each.isLoaded',
    'transfersLoading',
    function () {
      const isDir = this.get('file.isDir');
      const fileBlocks = this.get('fileBlocks');
      if (isDir || (fileBlocks && fileBlocks.every(fb => !get(fb, 'isLoading')))) {
        const providers = this.get('providers');
        if (providers && providers.every(p => get(p, 'isLoaded'))) {
          return this.get('transfersLoading') === false;
        }
      }
      return false;
    }
  ),
  
  /**
   * True if file is empty
   * @type {Ember.ComputedProperty<boolean>}
   */
  isFileEmpty: computed.equal('file.size', 0),

  /**
   * Record with current transfer records list 
   * @type {SpaceTransferList}
   */
  currentTransferList: computed.reads('space.currentTransferList'),
  
  scheduledTransferList: computed.reads('space.scheduledTransferList'),

  /**
   * Array of current transfers
   * @type {Array<Transfer>} with isLoaded Ember property
   */
  workingTransfers: computed(
    'workingTransfersDataLoaded',
    'scheduledTransferList.list.content.[]',
    'currentTransferList.list.content.[]',
    'lastTransfer',
    function () {
      if (this.get('workingTransfersDataLoaded')) {
        const scheduled = this.get('scheduledTransferList.list.content');
        const current = this.get('currentTransferList.list.content');
        const lastTransfer = this.get('lastTransfer');
        if (scheduled && current) {
          this.set('lastTransfer', null);
          return (lastTransfer ? [lastTransfer] : []).concat(scheduled.toArray(), current.toArray());
        }
      }
    }
  ),

  /**
   * True if each transfer is ready to be inserted into table, _but_ without
   * async dynamic data like transferred bytes or status
   * @type {Ember.ComputedProperty<boolean>}
   */
  workingTransfersDataLoaded: computed(
    'scheduledTransferList.isLoaded',
    'currentTransferList.isLoaded',
    'scheduledTransferList.list.content.isLoaded',
    'currentTransferList.list.content.isLoaded',
    function getCurrentTransfersDataLoaded() {
      return this.get('currentTransferList.isLoaded') === true &&
        this.get('scheduledTransferList.isLoaded') === true &&
        this.get('currentTransferList.list.content.isLoaded') === true &&
        this.get('scheduledTransferList.list.content.isLoaded') === true;
    }
  ),

  /**
   * @type {Ember.ComputedProperty<Array<Transfer>>}
   */
  fileTransfers: computed('workingTransfersDataLoaded', 'workingTransfers.[]', function () {
    const {
      workingTransfersDataLoaded,
      workingTransfers,
    } = this.getProperties(
      'workingTransfersDataLoaded',
      'workingTransfers'
    );
    const fileId = this.get('file.id');
    if (workingTransfersDataLoaded && workingTransfers) {
      return workingTransfers.filter(t => t.belongsTo('file').id() === fileId);
    }
  }),
  
  /**
   * @type {string}
   */
  transferIdsQuery: computed('fileTransfers.@each.id', function () {
    const fileTransfers = this.get('fileTransfers');
    if (fileTransfers) {
      return fileTransfers.map(t => get(t, 'id')).join(',');
    }
  }),
  
  /**
   * @type {Ember.ComputedProperty<Array<string>>|null}
   */
  currentMigrationSourceIds: computed('fileTransfers.@each.migrationSource', function () {
    /** @type {Ember.Array|undefined} */
    const fileTransfers = this.get('fileTransfers');
    if (fileTransfers) {
      return fileTransfers.map(t => get(t, 'migrationSource')).filter(s => s);
    }
  }),
  
  /**
   * Type of element that is presented in modal
   * One of: directory, rootDirectory, file
   * @type {Ember.ComputedProperty<string>}
   */
  fileType: computed('file.{isDir,parent}', function () {
    return this.get('file.isDir') ?
      (this.get('file.hasParent') ? 'directory' : 'rootDirectory')
      :
      'file';
  }),
  
  /**
   * Enable/disable updaters when transfers count for current file changes
   * @type {Ember.Observer}
   */
  observeTransfersCount: observer('fileTransfers.length', function () {
    const fileTransfers = this.get('fileTransfers');
    if (fileTransfers) {
      if (get(fileTransfers, 'length') === 0) {
        this._slowTransfersUpdater();
        this._slowDistributionUpdater();
      } else {
        this._fastTransfersUpdater();
        this._fastDistributionUpdater();
      }
    }
  }),

  //#region Transfers polling methods
  
  _initTransfersUpdater() {
    const {
      store,
      space,
    } = this.getProperties('store', 'space');
    return this.set('transfersUpdater', SpaceTransfersUpdater.create({
      store,
      space,
      isEnabled: true,
      scheduledEnabled: true,
      currentEnabled: true,
      completedEnabled: false,
    }));
  },

  _fastTransfersUpdater() {
    const transfersUpdater = this.get('transfersUpdater');
    if (transfersUpdater &&
      transfersUpdater.get('basePollingTime') !== FAST_POLLING_TIME) {
      transfersUpdater.set('basePollingTime', FAST_POLLING_TIME);
    }
  },

  _slowTransfersUpdater() {
    const transfersUpdater = this.get('transfersUpdater');
    if (transfersUpdater &&
      transfersUpdater.get('basePollingTime') !== SLOW_POLLING_TIME) {
      transfersUpdater.set('basePollingTime', SLOW_POLLING_TIME);
    }
  },
  
  _destroyTransfersUpdater() {
    const transfersUpdater = this.get('transfersUpdater');
    if (transfersUpdater) {
      transfersUpdater.destroy();
      this.set('transfersUpdater', null);
    }
    
  },
  
  //#endregion
  
  //#region File distribution polling methods
  
  /**
   * @returns {Promise<Array<FileDistribution>>}
   */
  fetchDistribution() {
    console.debug('component:modals/file-chunks: fetchDistribution');
    const fileId = this.get('file.id');
    this.get('store').query('file-distribution', { file: fileId }).then(
      (fbs) => {
        return this.set('fileBlocks', fbs);
      },
      (error) => {
        console.error('Error loading file distribution: ' + error.message);
        this.set('chunksModalError', error.message);
      }
    );
  },
  
  /**
   * This should be invoked only if distributionUpdater is empty
   * @returns {Looper|undefined} an object for polling file-distributions for file
   *  or nothing if distribution updater should not be created
   */
  _initDistributionUpdater() {
    const isDir = this.get('file.isDir');
    if (!isDir) {
      let distributionUpdater = this.get('distributionUpdater');
      assert(
        'distributionUpdater should be empty before init',
        !distributionUpdater || get(distributionUpdater, 'isDestroyed')
      );
      distributionUpdater = Looper.create({
        immediate: true,
        interval: SLOW_POLLING_TIME,
      });
      distributionUpdater
        .on('tick', () =>
          safeExec(this, 'fetchDistribution')
        );
      return this.set('distributionUpdater', distributionUpdater); 
    }
  },

  _fastDistributionUpdater() {
    const isDir = this.get('file.isDir');
    const distributionUpdater = this.get('distributionUpdater');
    if (!isDir) {
      if (distributionUpdater &&
        distributionUpdater.get('interval') !== FAST_POLLING_TIME) {
        distributionUpdater.set('interval', FAST_POLLING_TIME);
      }
    }
  },

  _slowDistributionUpdater() {
    const isDir = this.get('file.isDir');
    const distributionUpdater = this.get('distributionUpdater');
    if (!isDir) {
      if (distributionUpdater &&
        distributionUpdater.get('interval') !== SLOW_POLLING_TIME) {
        distributionUpdater.set('interval', SLOW_POLLING_TIME);
      }
    }
  },
  
  _destroyDistributionUpdater() {
    const distributionUpdater = this.get('distributionUpdater');
    if (distributionUpdater) {
      distributionUpdater.destroy();
      this.set('distributionUpdater', null);
    }
  },
  
  //#endregion
  
  init() {
    this._super(...arguments);
    if (this.get('open') == null) {
      this.set('open', false);
    }
  },

  /**
   * @override
   * Clean polling updaters
   */
  willDestroyElement() {
    try {
      // ensure updaters are destroyed (if destroying component without close)
      this._destroyTransfersUpdater();
      this._destroyDistributionUpdater();
    } finally {
      this._super(...arguments);
    }
  },
  
  forceUpdateTransfers(transfersUpdater) {
    return Promise.all([
        transfersUpdater.fetchScheduled(),
        transfersUpdater.fetchCurrent(true)
      ])
      .catch(error => {
        // TODO: i18n
        this.set('chunksModalError', 'Loading transfers data failed');
        throw error;
      })
      .finally(() => {
        this.set('transfersLoading', false);
      });
  },
  
  actions: {
    /**
     * File chunks modal component is placed all the time,
     * so this open actions works like a constructor.
     * On open, `file` property can change.
     */
    open() {
      this.set('providerMigrationsInvoked', A([]));
      
      const transfersUpdater = this._initTransfersUpdater();
      this.set('transfersLoading', true);
      this.forceUpdateTransfers(transfersUpdater);

      this._initDistributionUpdater();
      this.fetchDistribution();
      
      this.observeTransfersCount();
    },

    /**
     * Works as a pseudo-destructor
     */
    closed() {
      this._destroyDistributionUpdater();
      this._destroyTransfersUpdater();
      
      this.setProperties({
        file: null,
        fileBlocks: null,
        chunksModalError: null,
        migrationSource: null,
        providerMigrationsInvoked: undefined,
      });
      this.closedAction();
    },

    /**
     * Opens a menu with list of migration destination providers
     *
     * @param {Provider} sourceProvider a Provider that will source of migration
     */
    openMigrationOptions(sourceProvider) {
      run.next(() => {
        this.set('migrationSource', sourceProvider);
      });
    },

    /**
     * Closes a menu with list of migration destination providers
     */
    closeMigrationOptions() {
      this.set('migrationSource', null);
    },

    /**
     * Starts file migration from source provider to destination provider using backend
     * @param {File} file 
     * @param {string} source 
     * @param {string} destination 
     * @returns {Promise<Transfer|any>} resolves with saved transfer record
     *  migration start success
     */
    startMigration(file, source, destination) {
      this.set('migrationSource', null);
      const transfersUpdater = this.get('transfersUpdater');
      const providerMigrationsInvoked = this.get('providerMigrationsInvoked');
      providerMigrationsInvoked.pushObject(source);
      const transfer = this.get('store')
        .createRecord('transfer', {
          file,
          migration: true,
          migrationSource: source,
          destination: destination,
        });
      return transfer.save()
        // TODO: test it and make better fail messages
        .catch(error => {
          transfer.deleteRecord();
          this.set('chunksModalError', 'Failed to start file migration: ' + error.message);
          this.observeTransfersCount();
          throw error;
        })
        .then(() => transfersUpdater.fetchScheduled())
        .then(() => transfersUpdater.fetchCurrent())
        .then(() => {
          return this._fastTransfersUpdater();
        })
        .finally(() => {
          providerMigrationsInvoked.removeObject(source);
        });
    },

    /**
     * Starts replication - a transfer that has a target of placing file on
     * the destination provider; source providers are used automatically
     * @param {string} destination providerId
     */
    startReplication(destination) {
      const file = this.get('file');
      const transfersUpdater = this.get('transfersUpdater');
      const transfer = this.get('store')
        .createRecord('transfer', {
          file,
          migration: false,
          destination,
        });
      return transfer.save()
        .catch(error => {
          transfer.deleteRecord();
          this.set('chunksModalError', 'Failed to start file replication: ' + error.message);
          this.observeTransfersCount();
          throw error;
        })
        .then(transfer => {
          this.set('lastTransfer', transfer);
          return this.forceUpdateTransfers(transfersUpdater);
        })
        .then(() => this._fastTransfersUpdater());
    },
  },

});
