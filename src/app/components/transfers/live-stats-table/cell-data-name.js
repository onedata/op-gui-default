/**
 * Cell with file name for transfer row.
 * Cell component for `models-table` in `live-stats-table`.
 *
 * @module components/transfers/live-stats-table/cell-data-name
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import fileName from 'op-worker-gui/utils/file-name';

const {
  Component,
  computed,
  inject: { service },
} = Ember;

const i18nPrefix = 'components.transfers.liveTableStats.cellFileName.';

export default Component.extend({
  classNames: ['cell-data-name', 'cell-file-name'],
  i18n: service(),
  commonModals: service(),
  
  column: undefined,
  record: undefined,
  
  /**
   * Same as in `Transfer.dataSourceType`.
   * One of: dir, file, deleted, index, unknown
   */
  dataSourceType: computed.reads('record.dataSourceType'),
  filePath: computed.reads('record.path'),
  indexName: computed.reads('record.indexName'),
  totalFiles: computed.reads('record.totalFiles'),
  dataSourceRecordProxy: computed.reads('record.dataSourceRecord'),
  dataSourceRecord: computed.reads('record.dataSourceRecord.content'),
  space: computed.reads('record.space'),
  
  dataSourceName: computed('dataSourceType', 'indexName', 'filePath', function () {
    switch (this.get('dataSourceType')) {
      case 'file':
      case 'dir':
      case 'deleted':
        return fileName(this.get('filePath'));
      case 'index':
        return this.get('indexName');
      default:
        break;
    }
  }),
  
  deletedIsDir: computed('totalFiles', function deletedType() {
    return this.get('totalFiles') > 1;
  }),
  
  icon: computed('dataSourceType', 'deletedIsDir', function () {
    const {
      dataSourceType,
      deletedIsDir
    } = this.getProperties('dataSourceType', 'deletedIsDir');
    switch (dataSourceType) {
      case 'index':
        return 'index';
      case 'file':
        return 'file';
      case 'dir':
        return 'folder';
      case 'deleted':
        return deletedIsDir ? 'folder-deleted' : 'file-deleted';
      default:
        return 'unknown';
    }
  }),
  
  /**
   * @type {ComputedProperty<string>}
   */
  hint: computed('filePath', 'indexName', 'dataSourceType', 'deletedIsDir', function hint() {
    const {
      i18n,
      filePath,
      indexName,
      dataSourceType,
      deletedIsDir,
    } = this.getProperties('i18n', 'filePath', 'indexName', 'dataSourceType', 'deletedIsDir');
    
    switch (dataSourceType) {
      case 'file':
        return `${i18n.t(i18nPrefix + 'file')}: ${filePath}`;
      case 'dir':
        return `${i18n.t(i18nPrefix + 'dir')}: ${filePath}`;
      case 'deleted':
        return `${i18n.t(i18nPrefix + (deletedIsDir ? 'file' : 'dir'))}: ${filePath}`;
      case 'index':
        return `${i18n.t(i18nPrefix + 'index')}: ${indexName}`;
      default:
        break;
    }
  }),
  
  actions: {
    openIndexModal(mouseEvent) {
      const dbIndexId = this.get('record.dataSourceIdentifier');
      this.get('commonModals').openModal('dbIndex', {
        dbIndexId,
      });
      mouseEvent.preventDefault();
      mouseEvent.stopImmediatePropagation();
    },
  },
});
