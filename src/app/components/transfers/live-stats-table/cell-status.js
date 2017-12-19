/**
 * A cell component with transfer representation used by live-stats-table component.
 * 
 * @module components/transfers/live-stats-table/cell-status
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
  inject: { service },
} = Ember;

export default Component.extend({
  tagName: 'span',
  classNames: ['cell-status'],
  classNameBindings: ['_status'],
  i18n: service(),
  
  /**
   * ember-models-table record
   * @virtual
   * @type {Object}
   */
  record: undefined,
  
  /**
   * Transfer status.
   * @type {Ember.ComputedProperty<string>}
   */
  _status: computed.reads('record.status'),

  /**
   * Status icon.
   * @type {Ember.ComputedProperty<string>}
   */
  _icon: computed('_status', function () {
    switch (this.get('_status')) {
      case 'completed':
        return 'checkbox-filled';
      case 'skipped':
        return 'skipped';
      case 'cancelled':
        return 'cancelled';
      case 'failed':
        return 'checkbox-filled-x';
      case 'active':
      case 'finalizing':
        return 'update';
      case 'scheduled':
        return 'time';
    }
  }),
  
  /**
   * Status tooltip content.
   * @type {Ember.ComputedProperty<string>}
   */
  _hint: computed('_status', function () {
    const {
      i18n,
      _status,
    } = this.getProperties('i18n', '_status');
    return i18n.t(`components.transfers.liveTableStats.cellStatus.${_status}`);
  }),
});
