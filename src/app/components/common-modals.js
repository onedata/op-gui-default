import Ember from 'ember';

/**
 * Adds common modals to HTML, which can be opened with common-modals service.
 * @module components/common-modals
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  commonModals: Ember.inject.service(),

  /**
    Before opening modal, additional params may be required
    which can be used in specific modals
  */
  modalParams: {},

  registerInService: function() {
    this.set('commonModals.component', this);
  }.on('init'),

});
