/**
 * Imports a default adapter for application - currently Onedata adapter is used.
 * @module adapters/application
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import OnedataWebsocketAdapter from 'ember-cli-onedata-common/adapters/onedata-websocket';
import LSAdapter from 'op-worker-gui/adapters/application-localstorage';

import ENV from 'op-worker-gui/config/environment';

let ApplicationAdapter;
if (['test', 'localstorage'].indexOf(ENV.environment) !== -1) {
  ApplicationAdapter = LSAdapter;
} else {
  ApplicationAdapter = OnedataWebsocketAdapter;
}

export default ApplicationAdapter;
