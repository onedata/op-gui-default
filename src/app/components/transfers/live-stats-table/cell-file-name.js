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

const ICON_MAPPING = {
  file: 'file',
  dir: 'folder',
  deleted: 'close',
  unknown: 'sign-warning',
};

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
  
  fileName: computed('filePath', function () {
    return fileName(this.get('filePath'));
  }),
  
  icon: computed('fileType', function () {
    return ICON_MAPPING[this.get('fileType')];
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
