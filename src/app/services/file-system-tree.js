import Ember from 'ember';

/**
 * A global state of file browser
 * @module service/file-system-tree
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend(Ember.Evented, {
  store: Ember.inject.service(),

  spaces: null,
  selectedSpace: null,
  prevSelectedSpace: null,

  isLoading: null,

  /**
   * Stores ids of dirs that cannot be opened (eg. were rejected on request to backend).
   * @type Set<String>
   */
  failedDirs: null,

  init() {
    this._super();
    this.set('failedDirs', new Set());
  },

  /**
   * Opens a metadata editor for specified file in opened file browsers.
   * If file has no metadata, initialize it by creating a record (but not saving it).
   *
   * @param  {File} file
   */
  openMetadataEditor(file) {
    file.get('fileProperty').then(
      (metadata) => {
        if (!metadata) {
          metadata = this.get('store').createRecord('fileProperty', {
            file: file
          });
          file.set('fileProperty', metadata);
        }
      }
    );

    file.set('isEditingMetadata', true);
  },

  closeMetadataEditor(file) {
    file.set('isEditingMetadata', false);
  },

  rootDirs: Ember.computed('spaces.[]', function() {
    return this.get('spaces').mapBy('rootDir');
  }),

  rootSpaces: function() {
    let rootSpaces = {};
    this.get('spaces').forEach((s) => {
      rootSpaces[s.get('rootDir.id')] = s.get('id');
    });
    return rootSpaces;
  }.property('rootDirs.id'),

  spacesChanged: function() {
    console.debug(`FST: Spaces changed: len ${this.get('spaces.length')}, prev: ${this.get('prevSelectedSpace')}`);
    const dataSpaces = this.get('spaces');
    let newSpaceToSelect;
    if (!this.get('prevSelectedSpace') && this.get('spaces.length') > 0 &&
      dataSpaces.get('isUpdating') === false) {

      let defaultSpace = dataSpaces.find((s) => s.get('isDefault'));
      if (defaultSpace) {
        newSpaceToSelect = defaultSpace;
      } else {
        console.debug('No default data-space found - go to first data-space instead');
        const firstSpace = dataSpaces.sortBy('name').objectAt(0);
        if (firstSpace) {
          newSpaceToSelect = firstSpace;
        } else {
          console.debug('no data-spaces exist');
        }
      }

      this.set('prevSelectedSpace', this.get('selectedSpace'));
      this.set('selectedSpace', newSpaceToSelect);
    }
  }.observes('spaces', 'spaces.[]', 'spaces.@each.isDefault'),

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

  /**
   * Expands all directories (File) to root of directory tree from the File
   *
   * @param {File|Ember.ObjectProxy} file - a leaf file of the files tree
   * @returns {Promise} promise that will resolve when all dirs in tree to
   *  the file are expanded
   */
  expandDir(file) {
    return new Ember.RSVP.Promise((resolve) => {
      // using invocation from property, because file can be an ObjectProxy
      file.get('resolveDirsPath').apply(file).then(
        (path) => {
          let parentsLength = path.length - 1;
          for (let i=0; i<parentsLength; ++i) {
            path[i].set('isExpanded', true);
          }
          resolve();
        }
      );
      // TODO: this.rootDir should be the same as first element of path
      // TODO: check if dir to expand is child of previous dir?
      // TODO: should last dir in path be expanded?
    });

  },

  toggleMetadataEditor(file) {
    if (file.get('isEditingMetadata')) {
      this.closeMetadataEditor(file);
    } else {
      this.openMetadataEditor(file);
    }
  }
});
