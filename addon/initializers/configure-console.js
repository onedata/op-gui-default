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

function configure(config) {
  if (!config.debug) {
    console._debug = console.debug;
    console.debug = function() {};
  } else {
    console.debug('Debug messages in console enabled');
  }
}

export function initialize(application, ct) {
    if (application.getOnedataConfig) {
      try {
        application.deferReadiness();
        let configPromise = application.getOnedataConfig();
        configPromise.then(config => configure(config, ct));
        // when cannot fetch config file - use empty config
        configPromise.catch(() => configure({}));
        configPromise.finally(() => application.advanceReadiness());
      } catch (error) {
        // in case of unknown exception (this should not happen)
        application.advanceReadiness();
        throw error;
      }
    } else {
      console.error('No Ember.application.getOnedataConfig available - is fetch-config initializer invoked?');
    }
}

export default {
  name: 'configure-console',
  initialize: initialize,
  after: 'fetch-config'
};
