import Ember from 'ember';

/**
 * Adds knowledge about selected directory to data route.
 * 
 * @module controllers/onedata/data
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  fileBrowser: Ember.inject.service(),

  invalidRootDir: Ember.computed.alias('fileBrowser.invalidRootDir'),
  dir: Ember.computed.alias('fileBrowser.dir'),
});
