import Ember from 'ember';

const {
  inject
} = Ember;

/**
 * A global state and set of actions for file browser elements rendered in various
 * routes: data-files-list, data-files-list-toolbar.
 * It is created to avoid implementation of similar methods in separate components
 * or making heavy implementation of actions in model (components shares only File model).
 *
 * @module services/file-browser
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default Ember.Service.extend({
  fileSystemTree: inject.service(),
  eventsBus: inject.service(),

  invalidRootDir: false,

  /**
   * Current dir opened in file browser.
   */
  dir: null,

  init() {
    this._super(...arguments);
    this.get('eventsBus').on('dataFilesList:dirChanged', ({ dir }) => {
      this.set('dir', dir);
    });
  },

  // TODO: multiple select only with ctrl
  selectFile(file) {
    file.set('isSelected', !file.get('isSelected'));
  },

  // TODO: should open dialog or delegate to data files list (needs bind with data-files-list)
  // renameSelectedFile() {
  // },
});
