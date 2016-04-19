import Ember from 'ember';

/**
 * A global state of file browser
 * @module service/file-system-tree
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend(Ember.Evented, {
  store: Ember.inject.service('store'),

  spaces: null,
  selectedSpace: null,
  prevSelectedSpace: null,

  rootSpaces: function() {
    let rootSpaces = {};
    this.get('spaces').forEach((s) => {
      rootSpaces[s.get('rootDir.id')] = s.get('id');
    });
    return rootSpaces;
  }.property('spaces.@each.rootDir.id'),

  spacesChanged: function() {
    console.debug(`FST: Spaces changed: len ${this.get('spaces.length')}, prev: ${this.get('prevSelectedSpace')}`);
    if (!this.get('prevSelectedSpace') && this.get('spaces.length') > 0) {
      let defaultSpace = this.get('spaces').find((s) => s.get('isDefault'));
      console.debug('FST: spaces: ' + this.get('spaces').map((s) => s.get('isDefault')));
      if (defaultSpace) {
        console.debug(`FST: Will set new selectedSpace: ${defaultSpace.get('name')}`);
      } else {
        console.debug('FST: no selectedSpace!');
      }

      this.set('prevSelectedSpace', this.get('selectedSpace'));
      this.set('selectedSpace', defaultSpace);
    }
  }.observes('spaces', 'spaces.length', 'spaces.@each.isDefault'),

  getSpaceIdForFile(file) {
    if (file) {
      let parent = file.get('parent');
      if (parent.get('id')) {
        return this.getSpaceIdForFile(file.get('parent'));
      } else {
        return this.get('rootSpaces')[file.get('id')];
      }
    } else {
      return null;
    }
  },

  // TODO: cache of tree
  dirsPath(file) {
    return file ? file.dirsPath() : [];
  },

  expandDir(file) {
    return new Ember.RSVP.Promise((resolve) => {
      let path = this.dirsPath(file);
      // TODO: this.rootDir should be the same as first element of path
      // TODO: check if dir to expand is child of previous dir?
      // TODO: should last dir in path be expanded?
      let parentsLength = path.length - 1;
      for (let i=0; i<parentsLength; ++i) {
        path[i].set('isExpanded', true);
      }

      resolve();
    });

  }
});
