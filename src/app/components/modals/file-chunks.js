/**
 * Shows distribution of files among providers
 * @module components/modals/file-chunks
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';

const {
  Component,
  computed,
  get,
  run,
  inject: { service },
} = Ember;

export default Component.extend(PromiseLoadingMixin, {
  store: service(),

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
  fileForChunks: null,
  
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

  chunksModalError: null,
  
  /**
   * @type {Array<FileDistribution>}
   */
  fileBlocks: null,
  
  /**
   * @type {Provider}
   */
  migrationSource: undefined,
  
  // TODO: something is wrong because it is not sorted correctly
  fileBlocksSorting: ['getProvider.name'],
  fileBlocksSorted: computed.sort('fileBlocks', 'fileBlocksSorting'),
  
  providersSorted: computed.mapBy('fileBlocksSorted', 'getProvider'),
  
  /**
   * Convenience name for `fileForChunks`
   */
  file: computed.alias('fileForChunks'),
  
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
  
  fileTransfers: computed('currentTransfersDataLoaded', 'currentTransfers.[]', function () {
    const currentTransfersDataLoaded = this.get('curentTransfersDataLoaded');
    const currentTransfers = this.get('currentTransfers');
    const fileId = this.get('fileForChunks.id');
    if (currentTransfersDataLoaded && currentTransfers) {
      return currentTransfers.filter(t => t.belongsTo('file').id() === fileId);
    }
  }),
    
  actions: {
    open() {
      console.log('querying');
      let fileId = this.get('fileForChunks.id');
      // TODO: if fileId null...
      
      this.get('store').query('file-distribution', { file: fileId }).then(
        (fbs) => {
          this.set('fileBlocks', fbs);
        },
        (error) => {
          console.error('Error loading file blocks: ' + error.message);
          this.set('chunksModalError', error.message);
        }
      );
    },
    
    closed() {
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
        .save();
    },
    
    /**
     * Starts replication - a transfer that has a target of placing file on
     * the destination provider; source providers are used automatically
     * @param {string} destination providerId
     */
    startReplication(destination) {
      const file = this.get('file');
      this.get('store')
        .createRecord('transfer', {
          file,
          migration: false,
          destination,
        })
        .save();
    },
  },
  
});
