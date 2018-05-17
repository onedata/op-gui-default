/**
 * Cell that shows total processed files number.
 *
 * @module components/live-stats-table/cell-total-files
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  computed,
  getProperties,
  inject: {
    service,
  },
} = Ember;

const I18N_PREFIX = 'components.transfers.liveTableStats.cellTotalFiles.';

export default Ember.Component.extend({
  tagName: 'span',
  classNames: ['cell-total-files'],

  i18n: service(),

  /**
   * @type {Ember.ComptuedProperty<number>}
   */
  totalFiles: computed('record.{transferredFiled,invalidatedFiles}', function () {
    const {
      transferredFiles,
      invalidatedFiles,
    } = getProperties(this.get('record'), 'transferredFiles', 'invalidatedFiles');
    return (transferredFiles + invalidatedFiles) || 0;
  }),

  /**
   * @type {Ember.ComptuedProperty<string>}
   */
  tooltipTitle: computed('record.{transferredFiled,invalidatedFiles}', function () {
    const {
      transferredFiles,
      invalidatedFiles,
    } = getProperties(this.get('record'), 'transferredFiles', 'invalidatedFiles');
    const i18n = this.get('i18n');
    return `${transferredFiles || 0} ${i18n.t(I18N_PREFIX + 'transferred')}, ` + 
      `${invalidatedFiles || 0} ${i18n.t(I18N_PREFIX + 'invalidated')}`;
  }),
});
