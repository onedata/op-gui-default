import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';

const {
  computed: { readOnly },
  inject: { service },
} = Ember;

/**
 * Modal that displays info about this provider
 *
 * @module components/modals/about-modal
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(PromiseLoadingMixin, {
  session: service(),

  open: false,

  serviceVersion: readOnly('session.sessionDetails.serviceVersion'),
  providerName: readOnly('session.sessionDetails.providerName'),
  onezoneUrl: readOnly('session.sessionDetails.onezoneURL'),
});
