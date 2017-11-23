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
} = Ember;

export default Component.extend(PromiseLoadingMixin, {
  store: Ember.inject.service(),

  /** @abstract */
  modalId: null,

  open: false,

  /**
   * To inject.
   */
  chunksModalClosed: () => {},
  
  /** 
   * To inject.
   */
  fileForChunks: null,

  chunksModalError: null,
  isFileChunksModal: false,
  fileBlocks: null,
  
  fileBlocksSorting: ['provider.name'],
  fileBlocksSorted: computed.sort('fileBlocks', 'fileBlocksSorting'),

  actions: {
    open() {
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
        fileBlocks: null,
        chunksModalError: null
      });
      this.get('chunksModalClosed')();
    },
    
    startMigration(file, providerId) {
      this.get('store').createRecord('transfer', {
        file,
        migration: true,
        migrationSource: providerId,
      });
    },
    
    startReplication(file, providerId) {
      this.get('store').createRecord('transfer', {
        file,
        migration: false,
        destination: providerId,
      });
    },
  },
  
});
