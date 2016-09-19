import Ember from 'ember';

/**
 * Transit to root dir of current data-space.
 * @module routes/data/data-space/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  fileSystemTree: Ember.inject.service(),

  model() {
    return this.modelFor('onedata.data.data-space');
  },

  /** Transit to root dir of current DataSpace */
  afterModel(dataSpace) {
    let rootDir = dataSpace.get('rootDir');
    const rootDirId = rootDir.get('id');

    if (rootDir && rootDirId) {
      if (this.isDirOnFailedList(rootDirId)) {
        this.set('invalidRootDir', true);
      } else {
        this.set('invalidRootDir', false);
        console.debug(`Redirecting to root dir "${rootDir.get('id')}" of space "${dataSpace.get('id')}"`);
        Ember.run.scheduleOnce('afterRender', this, function() {
          this.transitionTo('onedata.data.data-space.dir', rootDir.get('id'));
        });
      }
    } else {
      console.warn(`Data space "${dataSpace.get('id')}" has no rootDir!`);
      this.set('invalidRootDir', true);
    }
  },

  isDirOnFailedList(dirId) {
    const failList = this.get('fileSystemTree.failedDirs');
    return failList ? failList.has(dirId) : false;
  },

  renderTemplate() {
    if (this.get('invalidRootDir')) {
      this.render('data.dataSpace.dir.error', {
        into: 'data',
        outlet: 'dir'
      });
    } else {
      this._super();
    }
  }
});
