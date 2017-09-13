import Ember from 'ember';
import snakeToCamel from 'op-worker-gui/utils/snake-to-camel';

/**
 * Provides API for open some modals rendered by common-modals component.
 * See components/common-modals for details.
 * @module services/common-modals
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  component: null,

  init() {
    this._super(...arguments);
  },

  openModal(type, params) {
    this.set('component.modalParams', params);
    this.set(openedProperty(type), true);
  },
  
  closeModal(type) {
    if (this.get('component')) {
      this.set('component.modalParams', null);
      this.set(openedProperty(type), false);
    }
  },
});

function openedProperty(type) {
  return 'component.' + snakeToCamel('opened-' + type);
}
