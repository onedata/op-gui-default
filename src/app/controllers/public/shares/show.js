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
   * @type {File}
   * @private
   */
  directory: null,

  pageTitle: Ember.computed('model', function() {
    return `"${this.get('model.name')}"`;
  }),

  /**
   * Watch change of Share, because we want to change current directory in
   * files browser.
   */
  containerDirChanged: Ember.observer('model.containerDir.content', function() {
    this.set('directory', this.get('model.containerDir.content'));
  }),

  actions: {
    openDirInBrowser(file) {
      this.set('directory', file);
    },
  }
});
