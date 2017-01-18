/* globals moment */
import Ember from 'ember';

import octalPermissionsToString from 'op-worker-gui/utils/octal-permissions-to-string';
import bytesToString from 'ember-cli-onedata-common/utils/bytes-to-string';

const {
  on,
  observer,
  computed,
  RSVP: {
    Promise
  }
} = Ember;

/**
 * FIXME jsdoc
 * @module mixins/models/file-runtime
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  errorNotifier: Ember.inject.service('errorNotifier'),
  oneproviderServer: Ember.inject.service(),

  /*** RUNTIME STATE */

  /// Runtime fields used to store state of file in application

  isExpanded: false,
  isSelected: false,
  isEditingMetadata: false,
  isNewlyCreated: false,

  /*** INIT */

  init() {
    this._super(...arguments);
    this.set('dirsPath', []);
  },

  updateDirsPath: on('init', observer('parent.name', function () {
    this.resolveDirsPath().then(data => this.set('dirsPath', data));
  })),

  /*** RELATION CHECKS */

  /// properties for checking presence of object in relation without relation resolve
  /// note that share and parent relations should be present in classes

  hasShare: Ember.computed('share.content', function () {
    return this.belongsTo('share').id() != null;
  }),
  hasParent: Ember.computed('parent.content', function () {
    return this.belongsTo('parent').id() != null;
  }),
  hasFileProperty: Ember.computed.reads('hasMetadata'),
  hasMetadata: Ember.computed('fileProperty.content', function () {
    return this.belongsTo('fileProperty').id() != null || !!this.get('fileProperty.content');
  }),

  /*** RUNTIME COMPUTED PROPERTIES */

  /**
   * Return true if this file is a dir and not all chilren are loaded from backend.
   * If this is not a dir, return undefined.
   */
  allChildrenLoaded: computed('totalChildrenCount', 'children.length', 'isDir', function () {
    if (this.get('isDir')) {
      return this.get('totalChildrenCount') <= this.get('children.length');
    }
  }),

  sizeHumanReadable: computed('size', function () {
    let size = this.get('size');
    return (size == null) ? '' : bytesToString(size);
  }),

  modificationMoment: computed('modificationTime', function () {
    let timestamp = this.get('modificationTime');
    return timestamp ? moment(timestamp * 1000).format('YYYY-MM-DD HH:MM') : '-';
  }),

  permissionsHumanReadable: computed('permissions', function () {
    let perms = this.get('permissions');
    return perms ? octalPermissionsToString(perms) : '';
  }),

  isDir: computed('type', function () {
    return this.get('type') === 'dir';
  }),

  isBroken: computed('type', function () {
    return this.get('type') === 'broken';
  }),

  isVisible: computed('parent.isExpanded', function () {
    var visible = this.get('parent.isExpanded');
    console.log('deselect(' + this.get('name') + '): ' +
      (this.get('isSelected') && !visible));
    if (this.get('isSelected') && !visible) {
      this.set('isSelected', false);
    }
    return visible;
  }),

  /**
   * A stringified path to file
   * @type {computed<string>}
   */
  path: computed('dirsPath.@each.name', function () {
    const dp = this.get('dirsPath');
    return dp && dp.mapBy('name').join('/');
  }),

  hasSubDirs: computed('children.@each.isDir', function () {
    if (this.get('isDir')) {
      return this.get('children').some(c => c.get('isDir'));
    } else {
      return false;
    }
  }),

  selectedFiles: computed('isDir', 'children.@each.isSelected', function () {
    if (this.get('isDir')) {
      return this.get('children').filterBy('isSelected');
    } else {
      return null;
    }
  }),

  selectedFilesType: computed('selectedFiles.@each.type', function () {
    const sf = this.get('selectedFiles');
    let firstType = sf.length > 0 ? sf[0].get('type') : undefined;
    if (sf.length > 0 && sf.every(f => f.get('type') === firstType)) {
      return firstType;
    } else {
      return 'mixed';
    }
  }),

  singleSelectedFile: computed('isDir', 'selectedFiles.[]', 'selectedFiles.firstObject',
    function () {
      if (this.get('isDir')) {
        let selected = this.get('selectedFiles');
        return selected.length === 1 ? selected.get('firstObject') : null;
      } else {
        return null;
      }
    }
  ),

  isSomeFileSelected: computed('isDir', 'selectedFiles.[]', function () {
    if (this.get('isDir')) {
      return this.get('selectedFiles.length') > 0;
    } else {
      return false;
    }
  }),

  /*** MODEL CLASS OVERRIDES */

  /**
   * Delete share when file delete succeeded
   * @override
   */
  destroyRecord() {
    const sharePromise = this.get('share');
    const destroyPromise = this._super(...arguments);
    destroyPromise.then(() => {
      if (sharePromise) {
        sharePromise.then(s => {
          if (s) {
            s.deleteRecord();
          }
        });
      }
    });
    return destroyPromise;
  },

  /**
  * First, destroys this file.
  * If it succeeds, delete (local cache) recursively all children.
  * 
  * @returns {RSVP.Promise} a promise from this file ``destroyRecord``
  */
  destroyRecursive() {
    let children = this.get('children');
    let file = this;
    let deleteChildren = function() {
      if (children) {
        children.forEach((child) => {
          child.deleteRecursive();
        });
      } else {
        console.debug('After destroy of ' + file.get('id') + ' there are no children');
      }
    };

    let destroyPromise = this.destroyRecord();
    destroyPromise.then(deleteChildren);
    destroyPromise.catch(() => file.rollbackAttributes());

    return destroyPromise;
  },

  deleteRecursive() {
    console.debug('Will delete recursive: ' + this.get('id'));
    let children = this.get('children');
    console.debug('Children to delete: ' + children.map((c) => c.get('i')));
    if (children && children.get('length') > 0) {
      children.forEach((child) => {
        child.deleteRecursive();
      });
    }
    console.debug('Deleting file: ' + this.get('id'));
    // remove self from parent children list - issues here TODO!
    // TODO: perfomance issues on big trees?
    let parent = this.get('parent');
    let parentChildren = parent.get('children');
    if (parent && parentChildren) {
      parent.set('children',
        parentChildren.filter((child) => child.get('id') !== this.get('id'))
      );
      this.set('parent', null);
    }
    this.deleteRecord();
    console.debug('File: ' + this.get('id') + ' isDeleted: ' + this.get('isDeleted'));
  },

  /*** UTILS */

  resetBrowserState() {
    this.setProperties({
      isExpanded: false,
      isSelected: false
    });
  },

  resetBrowserStateRecursive() {
    this.get('children').forEach(child => {
      child.resetBrowserStateRecursive();
    });
    this.resetBrowserState();
  },

  /**
   * Resolves an array with file parents, including the file.
   * The array is ordered from root dir to given file (from parents to children).
   *
   * @param file - a leaf file of path to find
   * @returns {RSVP.Promise} resolves with array of Files
   */
  resolveDirsPath() {
    let path = [this];
    return new Promise((resolve, reject) => {
      this.get('parent').then(
        (p) => {
          if (p) {
            p.resolveDirsPath().then((ppath) => {
              resolve(ppath.concat(path));
            });
          } else {
            resolve(path);
          }
        },

        () => {
          reject();
        }
      );
    });
  },

  /**
   * If this file is a dir, remove its selected files.
   * @returns {Map<RSVP.Promise>|String} Map: file -> destroy promise
   *                                     or String with error (if whole procedure fails)  
   */
  removeSelectedFiles() {
    // file -> destroy promise
    let fileDestroyPromises = new Map();
    if (this.get('isDir')) {
      this.get('selectedFiles').forEach(file => {
        fileDestroyPromises.set(file, file.destroyRecursive());
      });
    } else {
      console.error(`Called removeSelectedFiles on file that is not a directory ${this.get('id')}`);
      return 'Parent of files to remove is not a directory';
    }
    return fileDestroyPromises;
  },

  setSelectedFilesPermissions(permissions) {
    this.get('selectedFiles').forEach((file) => {
      file.set('permissions', parseInt(permissions));
      // TODO: handle errors
      file.save().then(() => {}, (failMessage) => {
        this.get('errorNotifier').handle(failMessage);
        file.rollback();
      });
    });
  },

  /**
    Creates file in this directory (only if this.get('isDir'))
    @param {String} type Type of file-object: 'file' or 'dir'
    @param {String} fileName
    @returns {RVSP.Promise} Promise that resolves with created file on save success
      and rejects with error on error:
      - resolve(file)
      - reject(error)
  */
  createFile(type, fileName) {
    if (!this.get('isDir')) {
      console.error(`Called createFile on file that is not a directory: ${this.get('id')}`);
    }

    const parentId = this.get('id');

    return new Ember.RSVP.Promise((resolve, reject) => {
      function handleFileCreationError(error) {
        try {
          console.error(
            `File with name "${fileName}" creation failed: ${JSON.stringify(error)};
File parent id: ${parentId}`
          );
        } finally {
          reject(error.message || error);
        }
      }

      const savePromise = this.get('oneproviderServer').createFile(fileName, parentId, type);
      savePromise.then(
        (data) => {
          console.debug(`File created with ID: ${data.fileId}`);
          resolve();
        },
        (error) => handleFileCreationError(error)
      );
    });
  }
});
