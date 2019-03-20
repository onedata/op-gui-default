import Ember from 'ember';

/**
 * Fetch JSON file with runtime front-end application config from ``/app-config.json``.
 * The file is currently generated automatically on project build
 * (see ``ember-cli-build`` in particular projects).
 * 
 * Creates ``Application.getOnedataConfig(): Promise<Object>`` method for accessing
 * the config in other modules.
 * 
 * @module initializers/fetch-config
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import ENV from 'op-worker-gui/config/environment';

export function initialize(application) {
  const rootURL = ENV.rootURL || '';
  application.getOnedataConfig = function() {
    return new Ember.RSVP.Promise((resolve, reject) => {
      $.ajax({
        dataSourceType: 'json',
        url: rootURL + 'app-config.json',
        success: function(data) {
          try {
            let config = {};
            config.debug = (data.debug === true);
            resolve(config);
          } catch (error) {
            reject();
          }
        },
        error() {
          reject();
        }
      });
    });
  };
}

export default {
  name: 'fetch-config',
  initialize: initialize
};
