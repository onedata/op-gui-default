/**
 * Adds support for back_forward query param used for detecting if back/forward
 * was used by user to get into this page.
 * 
 * @module controllers/onezone
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Controller,
} = Ember;

export default Controller.extend({
  queryParams: ['back_forward'],
});
