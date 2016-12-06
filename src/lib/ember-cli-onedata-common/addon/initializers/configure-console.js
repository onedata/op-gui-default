/**
 * Uses Onedata config read in ``fetch-config`` initializer to silent ``console.debug`` messages.
 * Requires ``fetch-config`` initializer!
 * Note, that this blocks application initialization until config is resolved/rejected.
 * 
 * @module initializers/configure-console
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function initialize(application) {
  application.deferReadiness();
  let configPromise = application.getOnedataConfig();
  configPromise.then(config => {
    if (!config.debug) {
      console._debug = console.debug;
      console.debug = function() {};
    } else {
      console.debug('Debug messages in console enabled');
    }
  });
  configPromise.finally(() => application.advanceReadiness());
}

export default {
  name: 'configure-console',
  initialize: initialize,
  after: 'fetch-config'
};
