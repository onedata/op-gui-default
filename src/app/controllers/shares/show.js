import Ember from 'ember';

export default Ember.Controller.extend({
  /**
   * Can be both File or Ember.ObjectProxy of File.
   * Please do not use this - use ``directory`` property instead.
   * @private
   */
  directory: null,

  // directory: Ember.computed('_dir', '_dir.content', {
  //   get() {
  //     const _dir = this.get('_dir');
  //     if (_dir != null) {
  //       if (_dir.constructor.toString().match(/model:file/)) {
  //         return _dir;
  //       } else {
  //         return _dir.content;
  //       }
  //     }
  //   },
  //   set(key, value) {
  //     this.set('_dir', value);
  //   }
  // }),

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
