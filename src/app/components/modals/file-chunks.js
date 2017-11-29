/**
 * Shows distribution of files among providers
 * @module components/modals/file-chunks
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';
import SpaceTransfersUpdater from 'op-worker-gui/utils/space-transfers-updater';
import Looper from 'ember-cli-onedata-common/utils/looper';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

const {
  Component,
  computed,
  observer,
  get,
  run,
  inject: { service },
  assert,
} = Ember;

const DISTRIBUTION_POLLING_TIME = 1000;

export default Component.extend(PromiseLoadingMixin, {
  store: service(),

  //#region External properties
  
  /**
   * @virtual
   * @type {string}
   */
  modalId: null,

  /**
   * @virtual
   * @type {File}
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
   * If true, first time loading data of transfers after modal open
   * @type {boolean}
   */
  transfersLoading: undefined,

  /**
   * How many times the transfer updater has been asked to start
   * @type {number}
   */
  _requestedUpdaters: 0,
  
  fileBlocksSorting: ['getProvider.name'],
  
  //#endregion

  fileBlocksSorted: computed.sort('fileBlocks', 'fileBlocksSorting'),

  providersSorted: computed.mapBy('fileBlocksSorted', 'getProvider'),

  /**
   * True if file is empty
   * @type {Ember.ComputedProperty<boolean>}
   */
  isFileEmpty: computed.equal('file.size', 0),

  currentTransferList: computed.reads('space.currentTransferList'),

  currentTransfers: computed.reads('currentTransferList.list.content'),

  currentTransfersDataLoaded: computed(
    'currentTransferList.isLoaded',
    'currentTransfers.@each.isLoaded',
    function getCurrentTransfersDataLoaded() {
      return this.get('currentTransferList.isLoaded') === true &&
        this.get('currentTransfers').every(t => get(t, 'isLoaded') === true);
    }
  ),

  /**
   * @type {Ember.ComputedProperty<Array<Transfer>>}
   */
  fileTransfers: computed('currentTransfersDataLoaded', 'currentTransfers.[]', function () {
    const currentTransfersDataLoaded = this.get('currentTransfersDataLoaded');
    const currentTransfers = this.get('currentTransfers');
    const fileId = this.get('file.id');
    if (currentTransfersDataLoaded && currentTransfers) {
      return currentTransfers.filter(t => t.belongsTo('file').id() === fileId);
    }
  }),

  /**
   * Enable/disable updaters when transfers count for current file changes
   * @type {Ember.Observer}
   */
  observeTransfersCount: observer('fileTransfers.length', function () {
    const fileTransfers = this.get('fileTransfers');
    if (fileTransfers) {
      if (get(fileTransfers, 'length') === 0) {
        this._stopTransfersUpdater();
        this._stopDistributionUpdater();
      } else {
        this._startTransfersUpdater();
        this._startDistributionUpdater();
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
      isEnabled: false,
      currentEnabled: true,
      completedEnabled: false,
    }));
  },

  _startTransfersUpdater() {
    if (this.get('transfersUpdater.isEnabled') !== true) {
      this.set('transfersUpdater.isEnabled', true);
    }
  },

  _stopTransfersUpdater() {
    if (this.get('transfersUpdater.isEnabled') === true) {
      this.set('transfersUpdater.isEnabled', false);
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
    console.warn('file-chunks: fetchDistribution');
    
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
   * @returns {Looper} an object for polling file-distributions for file
   */
  _initDistributionUpdater() {
    let distributionUpdater = this.get('distributionUpdater');
    assert(
      'distributionUpdater should be empty before init',
      !distributionUpdater || get(distributionUpdater, 'isDestroyed')
    );
    distributionUpdater = Looper.create({
      immediate: true,
    });
    distributionUpdater
      .on('tick', () =>
        safeExec(this, 'fetchDistribution')
      );
    return this.set('distributionUpdater', distributionUpdater);
  },

  _startDistributionUpdater() {
    if (this.get('distributionUpdater.interval') == null) {
      this.set('distributionUpdater.interval', DISTRIBUTION_POLLING_TIME);
    }
  },

  _stopDistributionUpdater() {
    if (this.get('distributionUpdater.interval') != null) {
      this.set('distributionUpdater.interval', null);
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
    
  willDestroyElement() {
    try {
      // ensure updaters are destroyed (if destroying component without close)
      this._destroyTransfersUpdater();
      this._destroyDistributionUpdater();
    } finally {
      this._super(...arguments);
    }
  },
  
  actions: {
    /**
     * File chunks modal component is placed all the time,
     * so this open actions works like a constructor.
     * On open, `file` property can change.
     */
    open() {
      const transfersUpdater = this._initTransfersUpdater();
      this.set('transfersLoading', true);
      transfersUpdater.fetchCurrent()
        .catch(error => {
          // TODO: i18n
          this.set('chunksModalError', 'Loading transfers data failed');
          throw error;
        })
        .finally(() => {
          this.set('transfersLoading', false);
        });

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
      return this.get('store')
        .createRecord('transfer', {
          file,
          migration: true,
          migrationSource: source,
          destination: destination,
        })
        .save()
        .then(( /* transfer */ ) => {
          this._startTransfersUpdater();
        })
        // TODO: test it and make better fail messages
        .catch(error => {
          this.set('chunksModalError', 'Failed to start file migration: ' + error.message);
          this._stopTransfersUpdater();
        });
    },

    /**
     * Starts replication - a transfer that has a target of placing file on
     * the destination provider; source providers are used automatically
     * @param {string} destination providerId
     */
    startReplication(destination) {
      const file = this.get('file');
      this._startTransfersUpdater();
      this.get('store')
        .createRecord('transfer', {
          file,
          migration: false,
          destination,
        })
        .save()
        .then(( /* transfer */ ) => {
          this._startTransfersUpdater();
        })
        .catch(error => {
          this.set('chunksModalError', 'Failed to start file replication: ' + error.message);
          this._stopTransfersUpdater();
        });
    },
  },

});
