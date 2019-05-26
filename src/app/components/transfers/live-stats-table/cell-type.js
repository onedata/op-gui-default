/**
 * A cell component with type of transfer used by live-stats-table component.
 * 
 * @module components/transfers/live-stats-table/cell-type
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
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
  classNames: ['cell-icon', 'cell-type'],
  classNameBindings: ['type'],
  i18n: service(),
  
  /**
   * ember-models-table record
   * @virtual
   * @type {Object}
   */
  record: undefined,
  
  /**
   * Transfer type (replication/migration/eviction).
   * @type {Ember.ComputedProperty<string>}
   */
  type: computed.reads('record.type'),

  /**
   * Status icon.
   * @type {Ember.ComputedProperty<string>}
   */
  _icon: computed('type', function _getIcon() {
    switch (this.get('type')) {
      case 'migration':
        return 'migrate';
      case 'replication':
        return 'replicate';
      case 'eviction':
        return 'evict';
      default:
        return 'unknown';
    }
  }),
  
  /**
   * Status tooltip content.
   * @type {Ember.ComputedProperty<string>}
   */
  _hint: computed('type', function () {
    const {
      i18n,
      type,
    } = this.getProperties('i18n', 'type');
    return i18n.t(`components.transfers.liveTableStats.cellType.${type}`);
  }),
});
