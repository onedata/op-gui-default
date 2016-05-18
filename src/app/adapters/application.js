/**
 * Imports a default adapter for application - currently Onedata adapter is used.
 * @module adapters/application
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import OnedataAdapter from './application-onedata';
import InMemoryAdapter from './application-in-memory';

import ENV from '../config/environment';

let ApplicationAdapter =
  (ENV.environment === 'test' ? InMemoryAdapter : OnedataAdapter);

export default ApplicationAdapter;
