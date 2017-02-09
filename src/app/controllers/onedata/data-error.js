import Ember from 'ember';
import SpacesErrorMixin from 'op-worker-gui/mixins/controllers/spaces-error';

const {
  Controller
} = Ember;

/**
 * Used when model for "data" (spaces list) could not be resolved or is not valid..
 * 
 * @module controllers/onedata/data-error
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Controller.extend(SpacesErrorMixin);
