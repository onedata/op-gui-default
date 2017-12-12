/**
 * Cell with file name for transfer row.
 * Cell component for `models-table` in `live-stats-table`.
 *
 * @module components/transfers/live-stats-table/cell-file-name
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

export default Component.extend({
  classNames: 'cell-file-name',
  i18n: service(),
  
  column: undefined,
  record: undefined,
  
  /**
   * Same as in `Transfer.fileType`.
   * One of: dir, file, deleted, unknown
   */
  fileType: computed.reads('record.fileType'),
  filePath: computed.reads('record.path'),
  totalFiles: computed.reads('record.totalFiles'),
  
  fileName: computed('filePath', function () {
    return fileName(this.get('filePath'));
  }),
  
  icon: computed('fileType', 'totalFiles', function () {
    const {
      fileType,
      totalFiles
    } = this.getProperties('fileType', 'totalFiles');
    switch (fileType) {
      case 'file':
        return 'file';
      case 'dir':
        return 'folder';
      case 'deleted':
        return totalFiles > 1 ? 'folder-deleted' : 'file-deleted';
      default:
        return 'unknown';
    }
  }),
  
  hint: computed('filePath', 'fileType', function () {
    const {
      i18n,
      filePath,
      fileType,
    } = this.getProperties('i18n', 'filePath', 'fileType');
    let hint = filePath;
    if (fileType === 'deleted') {
      hint += ` (${i18n.t('components.transfers.liveTableStats.cellFileName.deleted')})`;
    }
    return hint;
  }),
});
