/**
 * FIXME: jsdoc
 */

import Ember from 'ember';

const {
  Component,
  computed,
  inject: { service },
} = Ember;

export default Component.extend({
  tagName: 'span',
  classNames: 'cell-status',
  classNameBindings: ['_statusClass'],
  i18n: service(),
  
  record: undefined,
  
  _status: computed.reads('record.status'),

  _statusClass: computed('_status', function () {
    switch (this.get('_status')) {
      case 'completed':
        return 'success';
      case 'skipped':
      case 'cancelled':
      case 'failed':
        return 'failure';
      case 'active':
        return 'active';
      case 'scheduled':
        return 'inactive';
    }
  }),

  _icon: computed('_status', function () {
    switch (this.get('_status')) {
      case 'completed':
        return 'checkbox-filled';
      case 'skipped':
      case 'cancelled':
      case 'failed':
        return 'checkbox-filled-x';
      case 'active':
        return 'update';
      case 'scheduled':
        return 'time';
    }
  }),
  
  _hint: computed('_status', function () {
    const {
      i18n,
      _status,
    } = this.getProperties('i18n', '_status');
    return i18n.t(`components.transfers.liveTableStats.cellStatus.${_status}`);
  }),
});
