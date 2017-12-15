/**
 * A cell component with transfer actions
 * 
 * @module components/transfers/live-stats-table/cell-actions
 * @author Michal Borzecki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  inject: { service },
  computed,
  Object: EmberObject,
  A,
} = Ember;

const I18N_PREFIX = 'components.transfers.liveTableStats.cellActions.';

const ACTION_ICONS = {
  cancelTransfer: 'cancelled',
};

export default Component.extend({
  classNames: ['cell-actions'],
  i18n: service(),
  
  /**
   * ember-models-table record
   * @virtual
   * @type {Object}
   * - has actions property which is an array of objects
   */
  record: undefined,
    
  isCancelling: computed.reads('record.transfer.isCancelling'),
  
  menuActions: computed('record.actions', 'isCancelling', function () {
    const actions = this.get('record.actions');
    if (actions) {
      const {
        isCancelling,
        i18n,
        record,
      } = this.getProperties(
        'isCancelling',
        'i18n',
        'record'
      );
      return A(actions.map(({ id, action }) => EmberObject.create({
        title: i18n.t(I18N_PREFIX + id),
        // TODO: optimize - a function is created for each cell
        action: () => action(record),
        icon: ACTION_ICONS[id],
        disabled: isCancelling,
      })));
    }
  }),
});
