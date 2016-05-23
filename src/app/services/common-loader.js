import Ember from 'ember';

/**
 * Allows to globally control the singleton of common-loader component.
 * - Set 'isLoading' property to toggle loader.
 * - Set 'message' and (optional) 'messageSecondary' properties to set a message.
 * @module services/common-loader
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  component: null,
  // TODO: to extend - there can be many common-loader clients, so some can load and some not
  isLoading: false,
  message: null,
  messageSecondary: null,
});
