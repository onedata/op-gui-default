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
   * @type {Ember.ComputedProperty<number>}
   */
  totalFiles: computed('record.{replicatedFiles,invalidatedFiles}', function () {
    const {
      replicatedFiles,
      invalidatedFiles,
    } = getProperties(this.get('record'), 'replicatedFiles', 'invalidatedFiles');
    return (replicatedFiles + invalidatedFiles) || 0;
  }),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  tooltipTitle: computed('record.{replicatedFiles,invalidatedFiles}', function () {
    const {
      replicatedFiles,
      invalidatedFiles,
    } = getProperties(this.get('record'), 'replicatedFiles', 'invalidatedFiles');
    const i18n = this.get('i18n');
    return `${replicatedFiles || 0} ${i18n.t(I18N_PREFIX + 'replicated')}, ` + 
      `${invalidatedFiles || 0} ${i18n.t(I18N_PREFIX + 'invalidated')}`;
  }),
});
