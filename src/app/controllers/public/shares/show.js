import Ember from 'ember';

/**
 * Watch for model changes in public show share view.
 * @module controllers/public/shares/show
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  /**
   * Can be both File or Ember.ObjectProxy of File.
   * Please do not use this - use ``directory`` property instead.
   * @private
   */
  directory: null,

  /**
   * Watch change of Share, because we want to change current directory in
   * files browser.
   */
  modelChanged: Ember.observer('model', function() {
    this.set('directory', this.get('model.file'));
  }),

  actions: {
    openDirInBrowser(file) {
      this.set('directory', file);
    },
  }
});
