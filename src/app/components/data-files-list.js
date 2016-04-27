import Ember from 'ember';

/**
 * Files and subdirectories browser for single directory.
 * For file operations, see data-files-list-toolbar.
 * @module components/data-files-list
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  store: Ember.inject.service(),
  fileSystemTree: Ember.inject.service(),
  errorNotifier: Ember.inject.service(),
  fileBrowser: Ember.inject.service(),
  notify: Ember.inject.service(),
  fileUpload: Ember.inject.service(),

  classNames: ['data-files-list'],

  /** A parent directory to list its files */
  dir: null,

  // TODO: sorting switch in GUI
  filesSorting: ['type:asc', 'name:asc'],
  files: Ember.computed.alias('dir.children'),
  visibleFiles: Ember.computed.filter('files', (f) => f.get('id')),
  visibleFilesSorted: Ember.computed.sort('visibleFiles', 'filesSorting'),

  didInsertElement() {
    this.dirChanged();
    console.debug('Binding upload area for file list');
    this.get('fileUpload').assignDrop(this.$());
  },

  dirChanged: function() {
    this.set('fileUpload.dir', this.get('dir'));
  }.observes('dir'),

  actions: {
    /** If file - download it with backend; if dir - use route to browse other dir */
    openFile(file) {
      if (file.get('isDir')) {
        this.sendAction('openDirInBrowser', file.get('id'));
      } else {
        window.open(`/download/${file.get('id')}`, '_blank');
      }
    },

    // TODO: multiple select only with ctrl
    // TODO: select range with shift
    selectFile(file) {
      file.set('isSelected', !file.get('isSelected'));
    },
  }

});
