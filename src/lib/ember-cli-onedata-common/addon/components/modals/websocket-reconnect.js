/**
 * A modal that shows countdown to restore websocket connection
 * (controlled by session)
 * @module components/modals/websocket-reconnect
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import layout from 'ember-cli-onedata-common/templates/components/modals/websocket-reconnect';

const {
  Component,
  computed,
  inject: { service },
} = Ember;

export default Component.extend({
  layout,
  session: service(),
  
  reconnectModal: computed.alias('session.reconnectModal'),
  
  open: computed('reconnectModal.open', function () {
    return this.get('reconnectModal.open') || false;
  }),
  
  title: computed.readOnly('reconnectModal.title'),
  message: computed.readOnly('reconnectModal.message'),
  mode: computed.readOnly('reconnectModal.mode'),
  type: computed.readOnly('reconnectModal.type'),
  
  timeToReconnect: null,
  
  actions: {
    reconnectNow() {
      return this.get('session').reconnectNow();
    },
  }
});
