/**
 * Imports a default adapter for application - currently Onedata adapter is used.
 * @module adapters/application
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import OnedataAdapter from './application-onedata';
import LSAdapter from './application-localstorage';

import ENV from 'op-worker-gui/config/environment';

let ApplicationAdapter =
  (ENV.environment === 'test' ? LSAdapter : OnedataAdapter);

export default ApplicationAdapter;
