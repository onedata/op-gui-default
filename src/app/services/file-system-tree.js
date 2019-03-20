/**
 * A global state of file browser
 * @module service/file-system-tree
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';
import getDefaultSpace from 'op-worker-gui/utils/get-default-space';

const {
  Service,
  inject: { service },
  Evented,
  computed: { alias, reads },
  get,
  set,
} = Ember;

export default Service.extend(Evented, {
  store: service(),
  secondaryMenu: service(),
  session: service(),

  spaces: null,
  prevSelectedSpace: null,

  isLoading: null,

  selectedSpace: alias('secondaryMenu.activeSpace'),

  providerId: reads('session.sessionDetails.providerId'),

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
    // TODO: try to reload a file property if it failed before
    if (!file.get('metadataError')) {
      file.get('fileProperty')
        .then(
          (metadata) => {
            if (!metadata) {
              const fileType = file.get('constructor.modelName');
              const metadataType =
                (fileType === 'file-shared') ? 'filePropertyShared' : 'fileProperty';
              metadata = this.get('store').createRecord(metadataType, { file });
              file.set('fileProperty', metadata);
            }
          }
        )
        .catch(error => {
          file.set('metadataError', error);
        });
    }
    file.set('isEditingMetadata', true);
  },

  closeMetadataEditor(file) {
    file.set('isEditingMetadata', false);
  },

  rootDirs: Ember.computed('spaces.[]', function () {
    return this.get('spaces').mapBy('rootDir');
  }),

  rootSpaces: function () {
    let rootSpaces = {};
    this.get('spaces').forEach((s) => {
      rootSpaces[s.get('rootDir.id')] = s.get('id');
    });
    return rootSpaces;
  }.property('rootDirs.id'),

  spacesChanged: function () {
    console.debug(
      `FST: Spaces changed: len ${this.get('spaces.length')}, prev: ${this.get('prevSelectedSpace')}`
    );
    const dataSpaces = this.get('spaces');
    if (!this.get('prevSelectedSpace') && this.get('spaces.length') > 0 &&
      dataSpaces.get('isUpdating') === false) {
      getDefaultSpace(dataSpaces, this.get('providerId'))
        .then(newSpaceToSelect => {
          safeExec(this, 'setProperties', {
            prevSelectedSpace: this.get('selectedSpace'),
            selectedSpace: newSpaceToSelect,
          });
        });
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
          for (let i = 0; i < parentsLength; ++i) {
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
      set(file, 'isShowingInfo', false);
      this.openMetadataEditor(file);
    }
  },

  /**
   * @param {Array<models.File>} files 
   */
  toggleInfoViewer(files) {
    const openAll = files.some(f => !get(f, 'isShowingInfo'));
    files.forEach(f => {
      set(f, 'isShowingInfo', openAll);
      if (openAll && f.get('isEditingMetadata')) {
        this.closeMetadataEditor(f);
      }
    });
  },

  setSelectedSpace(space) {
    this.setProperties({
      prevSelectedSpace: this.get('selectedSpace'),
      selectedSpace: space,
    });
    return space;
  },

  backToPrevSpace() {
    const prevSpace = this.get('prevSelectedSpace');
    if (prevSpace) {
      return this.setSelectedSpace(prevSpace);
    }
  },
});
