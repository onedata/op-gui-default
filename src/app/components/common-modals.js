/**
 * Adds common modals to HTML, which can be opened with common-modals service.
 * Uses camelCase-keyed, boolean-valued properties: opened<ModalName>
 * to open a modals defined in this component template. See template for details.
 *
 * Also, renders a generic "info modal" which uses properties:
 * - infoModalTitle
 * - infoModalMessage
 * - infoModalOpened
 *
 * This component has corresponding service, which allows to open modal easily.
 * The component registers in service on init - so only one instance of component
 * should be created!
 *
 * See: services/common-modals
 *
 * @module components/common-modals
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  inject: { service },
} = Ember;

export default Component.extend({
  commonModals: service(),

  /**
    Before opening modal, additional params may be required
    which can be used in specific modals
  */
  modalParams: null,

  actions: {
    closeModal(type) {
      this.get('commonModals').closeModal(type);
    },
  },

});
