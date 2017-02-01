/**
 * Provides global ``window.App`` variable with Ember application object.
 * @module instance-initializers/global-app
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export function initialize(application) {
  window.App = application;
}

export default {
  name: 'global',
  initialize: initialize
};
