/**
 * Shows distribution of files among providers
 * @module components/modals/file-chunks
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';
import Looper from 'ember-cli-onedata-common/utils/looper';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import hasDuplicatedFileChunks from 'op-worker-gui/utils/has-duplicated-file-chunks';

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
  RSVP: { Promise, resolve, defer },
} = Ember;

const SLOW_POLLING_TIME = 10 * 1000;
const FAST_POLLING_TIME = 4 * 1000;

const I18N_PREFIX = 'components.dataFilesListToolbar.fileChunksModal.';

export default Component.extend(PromiseLoadingMixin, {
  classNames: ['file-chunks', 'file-chunks-modal'],
  
  session: service(),
  store: service(),
  oneproviderServer: service(),
  i18n: service(),

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

  /**
   * List of provider ids for which invalidation was invoked, but not yet started
   * @type {Ember.Array<string>}
   */
  providerInvalidationsInvoked: undefined,
  
  fileBlocksSorting: ['getProvider.name'],
    
  confirmOperationType: null,
  
  confirmOpened: false,
  
  /**
   * @type {Looper}
   */
  _fileTransfersWatcher: undefined,
  
  /**
   * Number of ended transfers for this file. Updated by `_updateFileTransfers`
   * @type {number}
   */
  _endedTransfersCount: 0,
  
  /**
   * Max number of ended transfers that can be fetched for transfer
   * @type {Ember.ComputedProperty<number>}
   */
  _historyLimitPerFile: computed.reads('session.sessionDetails.config.transfersHistoryLimitPerFile'),
  
  /**
   * True if the `_endedTransfersCount` reached history limit
   * @type {boolean}
   */
  _endedTransfersMore: computed('_historyLimitPerFile', '_endedTransfersCount', function () {
    const {
      _historyLimitPerFile,
      _endedTransfersCount,
    } = this.getProperties('_historyLimitPerFile', '_endedTransfersCount');
    return _endedTransfersCount >= _historyLimitPerFile;
  }),
  
  //#endregion
  
  confirmOperationBtnLabel: computed('confirmOperationType', function confirmOperationBtnLabel() {
    const i18n = this.get('i18n');
    const confirmOperationType = this.get('confirmOperationType');
    return `${i18n.t(I18N_PREFIX + 'confirmStart')} ${i18n.t(I18N_PREFIX + 'confirmType.' + confirmOperationType)}`;
  }),
  
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
   * Set with providerIds of providers, which have a working invalidation
   * transfer for file.
   * @type {Ember.ComputedProperty<Set<string>>}
   */
  providersIdsWithInvalidation: computed('fileTransfers.[]', function () {
    const fileTransfers = this.get('fileTransfers');
    const providers = new Set();
    if (fileTransfers) {
      fileTransfers.forEach(transfer => {
        if (get(transfer, 'type') === 'invalidation') {
          providers.add(get(transfer, 'invalidatingProvider'));
        }
      });
    }
    return providers;
  }),

  /**
   * Returns mapping: providerId -> boolean value. Value is true if there are
   * some blocks available for invalidation.
   * @type {Ember.ComputedProperty<Object>}
   */
  isInvalidationPossible: computed(
    'fileDistributionsSorted.@each.{blocks,neverSynchronized}',
    'providersIdsWithInvalidation',
    function () {
      const {
        fileDistributionsSorted,
        file,
        providersIdsWithInvalidation,
      } = this.getProperties(
        'fileDistributionsSorted',
        'file',
        'providersIdsWithInvalidation'
      );
      if (fileDistributionsSorted) {
        const fileChunksArray = fileDistributionsSorted.map(fd => ({
          providerId: get(fd, 'provider'),
          blocks: !get(fd, 'neverSynchronized') ? get(fd, 'blocks') : [],
        }));
        const invalidationPossible = {};
        fileChunksArray.forEach(chunks => {
          const providerId = get(chunks, 'providerId');
          invalidationPossible[providerId] = hasDuplicatedFileChunks(
            file.get('size'),
            get(chunks, 'blocks'),
            fileChunksArray.filter(fc =>
              fc !== chunks &&
              !providersIdsWithInvalidation.has(get(fc, 'providerId'))
            ).map(fc => get(fc, 'blocks'))
          );
        });
        return invalidationPossible;
      }
    }
  ),

  /**
   * True if there is at least one waiting or ongoing transfer for this file
   * @type {Ember.ComputedProperty<boolean>}
   */
  areTransfersInProgress: computed.reads('fileTransfers.length'),
  
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
   * Array of current transfers
   * @type {Array<Transfer>} with isLoaded Ember property
   */
  workingTransfers: undefined,

  /**
   * True if each transfer is ready to be inserted into table, _but_ without
   * async dynamic data like transferred bytes or status
   * @type {Ember.ComputedProperty<boolean>}
   */
  workingTransfersDataLoaded: true,

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
   * @type {Ember.ComputedProperty<Array<string>>|null}
   */
  currentMigrationSourceIds: computed(
    'fileTransfers.@each.invalidatingProvider',
    function () {
      /** @type {Ember.Array|undefined} */
      const fileTransfers = this.get('fileTransfers');
      if (fileTransfers) {
        return fileTransfers.map(t => get(t, 'invalidatingProvider')).filter(s => s);
      }
    }
  ),
  
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
        this._slowTransfersWatcher();
        this._slowDistributionUpdater();
      } else {
        this._fastTransfersWatcher();
        this._fastDistributionUpdater();
      }
    }
  }),

  //#region Transfers polling methods
  
  _initTransfersWatcher() {
    const _fileTransfersWatcher = Looper.create({
      immediate: true,
      interval: SLOW_POLLING_TIME,
    });
    this.set('_fileTransfersWatcher', _fileTransfersWatcher);
    _fileTransfersWatcher.on('tick', () => {
      safeExec(this, '_updateFileTransfers');
    });
  },

  _fastTransfersWatcher() {
    const _fileTransfersWatcher = this.get('_fileTransfersWatcher');
    if (_fileTransfersWatcher &&
      _fileTransfersWatcher.get('interval') !== FAST_POLLING_TIME) {
        _fileTransfersWatcher.set('interval', FAST_POLLING_TIME);
    }
  },

  _slowTransfersWatcher() {
    const _fileTransfersWatcher = this.get('_fileTransfersWatcher');
    if (_fileTransfersWatcher &&
      _fileTransfersWatcher.get('interval') !== SLOW_POLLING_TIME) {
        _fileTransfersWatcher.set('interval', SLOW_POLLING_TIME);
    }
  },
  
  _destroyTransfersWatcher() {
    const _fileTransfersWatcher = this.get('_fileTransfersWatcher');
    if (_fileTransfersWatcher) {
      _fileTransfersWatcher.destroy();
      this.set('_fileTransfersWatcher', null);
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
    const _fileTransfersWatcher = this.get('distributionUpdater');
    if (!isDir) {
      if (_fileTransfersWatcher &&
        _fileTransfersWatcher.get('interval') !== SLOW_POLLING_TIME) {
        _fileTransfersWatcher.set('interval', SLOW_POLLING_TIME);
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
      this._destroyTransfersWatcher();
      this._destroyDistributionUpdater();
    } finally {
      this._super(...arguments);
    }
  },
  
  forceUpdateTransfers() {
    return this._updateFileTransfers()
      .catch(error => {
        // TODO: i18n
        this.set('chunksModalError', 'Loading transfers data failed');
        throw error;
      })
      .finally(() => {
        this.set('transfersLoading', false);
      });
  },
  
  /**
   * Update all transfer current stats for file transfers
   * @returns {Promise<Array<TransferCurrentStat>>}
   */
  _updateFileTransfers() {
    const {
      store,
      oneproviderServer,
    } = this.getProperties('store', 'oneproviderServer');
    return oneproviderServer
      .getTransfersForFile(this.get('file.id'), 'count')
      .then(({ ongoing, ended }) => {
        safeExec(this, 'set', '_endedTransfersCount', ended);
        return Promise.all(ongoing.map(transferId => store.findRecord('transfer', transferId)));
      })
      .then(transfers => safeExec(this, 'set', 'workingTransfers', transfers))
      .then(transfers => transfers.map(transfer => transfer.belongsTo('currentStat').reload()));
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
      const providerMigrationsInvoked = this.get('providerMigrationsInvoked');
      providerMigrationsInvoked.pushObject(source);
      const transfer = this.get('store')
        .createRecord('transfer', {
          file,
          invalidatingProvider: source,
          replicatingProvider: destination,
        });
      return transfer.save()
        // TODO: test it and make better fail messages
        .catch(error => {
          transfer.deleteRecord();
          this.set('chunksModalError', 'Failed to start file migration: ' + error.message);
          this.observeTransfersCount();
          throw error;
        })
        .then(() => this.forceUpdateTransfers())
        .then(() => {
          return this._fastTransfersWatcher();
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
      const transfer = this.get('store')
        .createRecord('transfer', {
          file,
          replicatingProvider: destination,
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
          return this.forceUpdateTransfers();
        })
        .then(() => this._fastTransfersWatcher());
    },

    startInvalidation(source) {
      const {
        file,
        providerInvalidationsInvoked
      } = this.getProperties(
        'file',
        'providerInvalidationsInvoked'
      );
      providerInvalidationsInvoked.pushObject(source);
      const transfer = this.get('store')
        .createRecord('transfer', {
          file,
          invalidatingProvider: source,
        });
      return transfer.save()
        .catch(error => {
          transfer.deleteRecord();
          this.set('chunksModalError', 'Failed to start file invalidation: ' + error.message);
          this.observeTransfersCount();
          throw error;
        })
        .then(transfer => {
          this.set('lastTransfer', transfer);
          return this.forceUpdateTransfers();
        })
        .finally(() => {
          providerInvalidationsInvoked.removeObject(source);
        });
    },
    
    showConfirmOperation(confirmOperationType) {
      const confirmationDeferred = defer();
      this.setProperties({
        confirmOpened: true,
        confirmOperationType,
        confirmationDeferred,
      });
      return confirmationDeferred.promise;
    },
    
    startConfirmOperation(transfersPending, type, fun) {
      if (transfersPending) {
        return this.showConfirmOperation(type)
          .then(confirmed => {
            if (confirmed) {
              return fun();
            } else {
              return resolve();
            }
          });
      } else {
        return fun();
      }
    },
    
    actions: {
      confirmOperation(confirmed) {
        const confirmationDeferred = this.get('confirmationDeferred');
        confirmationDeferred.resolve(confirmed);
        this.setProperties({
          confirmOpened: false,
          confirmOperationType: null,
          confirmationDeferred: null,
        });
      },
      
      /**
       * File chunks modal component is placed all the time,
       * so this open actions works like a constructor.
       * On open, `file` property can change.
       */
      open() {
        this.set('providerMigrationsInvoked', A([]));
        this.set('providerInvalidationsInvoked', A([]));

        this._initTransfersWatcher();
        this.set('transfersLoading', true);
        this.forceUpdateTransfers();

        this._initDistributionUpdater();
        this.fetchDistribution();

        this.observeTransfersCount();
      },

      /**
       * Works as a pseudo-destructor
       */
      closed() {
        this._destroyDistributionUpdater();
        this._destroyTransfersWatcher();

        this.setProperties({
          file: null,
          fileBlocks: null,
          chunksModalError: null,
          migrationSource: null,
          providerMigrationsInvoked: undefined,
          providerInvalidationsInvoked: undefined,
        });
        this.closedAction();
      },

      /**
       * Opens a menu with list of migration destination providers
       *
       * @param {Provider} sourceProvider a Provider that will source of migration
       */
      openMigrationOptions(sourceProvider, { transfersPending }) {
        run.next(() => {
          this.set('migrationSource', sourceProvider);
          this.set('migrationTransfersPending', transfersPending);
        });
      },

      /**
       * Closes a menu with list of migration destination providers
       */
      closeMigrationOptions() {
        this.set('migrationSource', null);
        this.set('migrationTransfersPending', null);
      },

      startMigration(file, source, destination, { transfersPending }) {
        return this.startConfirmOperation(
          transfersPending,
          'migrate',
          () => this.startMigration(file, source, destination)
        );
      },

      startReplication(destination, { transfersPending }) {
        return this.startConfirmOperation(
          transfersPending,
          'replicate',
          () => this.startReplication(destination)
        );
      },

      startInvalidation(source, { transfersPending }) {
        return this.startConfirmOperation(
          transfersPending,
          'invalidate',
          () => this.startInvalidation(source)
        );
      },
    },

});
