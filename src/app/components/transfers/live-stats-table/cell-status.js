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
  classNameBindings: ['_isStatusSuccess:success:failure'],
  i18n: service(),
  
  record: undefined,
  
  _status: computed.reads('record.status'),

  _isStatusSuccess: computed.equal('_status', 'completed'),

  _icon: computed('_isStatusSuccess', function () {
    return this.get('_isStatusSuccess') ? 'checkbox-filled' : 'checkbox-filled-x';
  }),
  
  _hint: computed('_status', function () {
    const {
      i18n,
      _status,
    } = this.getProperties('i18n', '_status');
    return i18n.t(`components.transfers.liveTableStats.cellStatus.${_status}`);
  }),
});
