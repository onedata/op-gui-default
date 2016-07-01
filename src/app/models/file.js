/**
 * This is a prototype model representing a file in file browser.
 * New implementation with data-space support.
 * @module models/file
 * @author Łukasz Opioła
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';
import Ember from 'ember';
import octalPermissionsToString from '../utils/octal-permissions-to-string';
/* globals moment */

export default DS.Model.extend({
  errorNotifier: Ember.inject.service('errorNotifier'),
  notify: Ember.inject.service('notify'),

  name: DS.attr('string'),
  /**
    Specifies is this object a regular file ("file") or directory ("dir")
    To check if it is a dir please use "isDir" property.
  */
  type: DS.attr('string'),
  parent: DS.belongsTo('file', {inverse: 'children', async: true}),
  children: DS.hasMany('file', {inverse: 'parent', async: true}),

  modificationTime: DS.attr('number'),
  size: DS.attr('number'),
  permissions: DS.attr('number'),
  acl: DS.hasMany('file-access-control', {inverse: 'file', async: true}),

  isExpanded: false,
  isSelected: false,

  // TODO: implement B, MB, GB, TODO: move to helper
  sizeHumanReadable: function() {
    let bytes = this.get('size');
    if (!bytes && bytes !== 0) {
      return '';
    }

    let number = bytes;
    let unit = 'B';
    if (bytes > 1073741824) {
      unit = 'GB';
      number = bytes/1073741824;
    } else if (bytes >= 1048576) {
      unit = 'MB';
      number = bytes/1048576;
    } else if (bytes >= 1024) {
      unit = 'KB';
      number = bytes/1024;
    }
    return `${Math.round(number * 100) / 100} ${unit}`;
  }.property('size'),

  modificationMoment: function() {
    let timestamp = this.get('modificationTime');
    return timestamp ? moment(timestamp * 1000).format('YYYY-MM-DD HH:MM') : '-';
  }.property('modificationTime'),

  permissionsHumanReadable: function() {
    let perms = this.get('permissions');
    return perms ? octalPermissionsToString(perms) : '';
  }.property('permissions'),

  isDir: function () {
    return this.get('type') === 'dir';
  }.property('type'),

  isBroken: function () {
    return this.get('type') === 'broken';
  }.property('type'),

  resetBrowserState() {
    this.set('isExpanded', false);
    this.set('isSelected', false);
  },

  resetBrowserStateRecursive() {
    this.get('children').forEach((child) => child.resetBrowserStateRecursive());
    this.resetBrowserState();
  },

  // TODO: doc, destroy, not destroyRecord!
  destroyRecursive() {
    // TODO: onsuccess onfailure...
    let children = this.get('children');
    let file = this;
    let deleteChildren = function() {
      file.get('notify').success('File removed');
      if (children) {
        children.forEach((child) => {
          child.deleteRecursive();
        });
      } else {
        console.debug('After destroy of ' + file.get('id') + ' there is no children');
      }
    };

    this.destroyRecord().then(deleteChildren, (failMessage) => {
      file.get('errorNotifier').handle(failMessage);
      file.rollbackAttributes();
    });
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

  isVisible: function () {
    var visible = this.get('parent.isExpanded');
    console.log('deselect(' + this.get('name') + '): ' +
      (this.get('isSelected') && !visible));
    if (this.get('isSelected') && !visible) {
      this.set('isSelected', false);
    }
    return visible;
  }.property('parent.isExpanded'),

  /// Utils

  /**
   * Returns array with file parents, including the file.
   * The array is ordered from root dir to given file (from parents to children).
   *
   * @param file - a leaf file of path to find
   * @returns {Array} array of Files
   */
  dirsPath() {
    let path = [this];
    let parent = this.get('parent');
    while (parent && parent.get('id')) {
      path.unshift(parent);
      parent = parent.get('parent');
    }
    console.debug(`Computed path for file ${this.get('id')}: ${JSON.stringify(path)}`);
    return path;
  },

  // TODO: may not update properly
  path: function() {
    return this.dirsPath().map(f => f.get('name')).join('/');
  }.property('parent'),

  // TODO: move directory utils to mixin
  /// Directory utils

  hasSubDirs: function() {
    if (this.get('isDir')) {
      return this.get('children').filter((child) => child.get('isDir'))
        .length > 0;
    } else {
      return false;
    }
  }.property('children.@each.isDir'),

  selectedFiles: function() {
    if (this.get('isDir')) {
      return this.get('children').filter((file) => file.get('isSelected'));
    } else {
      return null;
    }
  }.property('children.@each.isSelected'),

  singleSelectedFile: function() {
    if (this.get('isDir')) {
      let selected = this.get('selectedFiles');
      return selected.length === 1 ? selected[0] : null;
    } else {
      return null;
    }
  }.property('selectedFiles'),

  isSomeFileSelected: function() {
    if (this.get('isDir')) {
      return this.get('selectedFiles.length') > 0;
    } else {
      return false;
    }
  }.property('selectedFiles'),

  removeSelectedFiles() {
    if (this.get('isDir')) {
      this.get('selectedFiles').forEach((file) => {
        file.destroyRecursive();
      });
    } else {
      console.error(`Called removeSelectedFiles on file that is not a directory ${this.get('id')}`);
    }
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

    let record = this.get('store').createRecord('file', {
      name: fileName,
      parent: this,
      type: type
    });
    return new Ember.RSVP.Promise((resolve, reject) => {
      let savePromise = record.save();
      savePromise.then(
        () => {
          resolve(record);
        },
        (error) => {
          // advanced error notifier moved up
          // this.get('errorNotifier').handle(failMessage);
          try {
            console.error(
`File with name "${record.get('name')}" creation failed: ${JSON.stringify(error)};
File parent id: ${this.get('id')}`
            );
            record.destroyRecord();
          } finally {
            reject(error.message || error);
          }
        }
      );
    });
  },

});
