import Ember from 'ember';

import cutDirsPath from 'op-worker-gui/utils/cut-dirs-path';

export default Ember.Component.extend({
  classNames: ['file-breadcrumbs'],

  /**
   * File, which path to will be presented.
   * Can be a directory of course.
   * @type {File}
   */
  file: null,

  dirsPath: Ember.computed('file.dirsPath.[]', 'rootDir', function() {
    const dpath = this.get('file.dirsPath');
    if (dpath) {
      const rdir = this.get('rootDir');
      return rdir ? cutDirsPath(dpath, rdir) : dpath;
    } else {
      return null;
    }
  }),

  /**
   * Optional: if provided, breadcrumbs will use ``rootDir`` as a root of path
   * (if provided)
   * @type {File}
   */
  rootDir: null,

  isLoading: Ember.computed('dirsPath', function() {
    return !this.get('dirsPath');
  }),

  actions: {
    changeDir(file) {
      this.sendAction('changeDir', file);
    }
  }
});
