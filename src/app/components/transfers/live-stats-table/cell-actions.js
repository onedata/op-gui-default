/**
 * A cell component with transfer actions
 * 
 * @module components/transfers/live-stats-table/cell-actions
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
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
  rerunTransfer: 'rerun',
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
  
  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  isCancelling: computed.reads('record.transfer.isCancelling'),

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  transferFilesDeleted: computed.equal('record.transfer.fileType', 'deleted'),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  message: computed.reads('record.actionMessage'),

  /**
   * @type {Ember.ComputedProperty<string>}
   */
  messageType: computed.reads('record.actionMessageType'),
  
  /**
   * @type {Ember.ComputedProperty<Ember.A<EmberObject>>}
   */
  menuActions: computed(
    'record.actions',
    'record.isRerunning',
    'isCancelling',
    'transferFilesDeleted',
    function () {
      const actions = this.get('record.actions');
      if (actions) {
        const {
          i18n,
          record,
        } = this.getProperties(
          'i18n',
          'record'
        );
        return A(actions.map(({ id, action }) => EmberObject.create({
          title: i18n.t(I18N_PREFIX + id),
          // TODO: optimize - a function is created for each cell
          action: () => action(record),
          icon: ACTION_ICONS[id],
          disabled: this.isActionDisabled(id),
        })));
      }
    }
  ),

  /**
   * Returns true if action cannot be performed for this transfer
   * @param {string} actionId
   * @returns {boolean}
   */
  isActionDisabled(actionId) {
    const {
      record,
      isCancelling,
      transferFilesDeleted,
    } = this.getProperties('record', 'isCancelling', 'transferFilesDeleted');
    switch (actionId) {
      case 'cancelTransfer':
        return isCancelling;
      case 'rerunTransfer':
        return transferFilesDeleted || record.get('isRerunning');
    }
  }
});
