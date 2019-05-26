/**
 * Provides API for open some modals rendered by common-modals component.
 * See components/common-modals for details.
 * @module services/common-modals
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import snakeToCamel from 'op-worker-gui/utils/snake-to-camel';

export default Ember.Service.extend({
  init() {
    this._super(...arguments);
  },

  openModal(type, params) {
    this.set('modalParams', params);
    this.set(this._openedProperty(type), true);
  },

  closeModal(type) {
    this.set('modalParams', null);
    this.set(this._openedProperty(type), false);
  },

  /**
   * @param {string} type type of modal
   * @returns {string} 
   */
  _openedProperty(type) {
    return snakeToCamel('opened-' + type);
  },
});
