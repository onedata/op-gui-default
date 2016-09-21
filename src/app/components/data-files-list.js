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
  commonLoader: Ember.inject.service(),
  i18n: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  messageBox: Ember.inject.service(),

  classNames: ['data-files-list'],

  /// Options, features
  uploadEnabled: true,
  breadcrumbsEnabled: false,
  publicMode: false,

  /**
   * Optional: if specified, breadcrumbs will have this dir as a root.
   * Otherwise, breadcrumbs will display full parents path.
   * @type {File}
   */
  rootDir: null,


  /** A parent directory to list its files */
  dir: null,

  // TODO: sorting switch in GUI
  filesSorting: ['type:asc', 'name:asc'],
  files: Ember.computed.alias('dir.children'),
  visibleFiles: function() {
    return this.get('files').filter((f) => f.get('isLoaded') && !f.get('isBroken'));
  }.property('files', 'files.[]', 'files.@each.isLoaded'),
  visibleFilesSorted: Ember.computed.sort('visibleFiles', 'filesSorting'),

  dirIsEmpty: function() {
    return !this.get('visibleFiles') || this.get('visibleFiles.length') === 0;
  }.property('visibleFiles.length'),

  /**
   * A file browser loading state. Not only checks if files model is loaded
   * but also checks if fileUpload is locked.
   */
  isLoadingFiles: function() {
    return this.get('fileUpload.locked') ||
      !!this.get('files').any((f) => !f.get('isLoaded')) ||
      this.get('files.isUpdating');
  }.property('files.isUpdating', 'files', 'files.[]', 'files.@each.isLoaded', 'fileUpload.locked'),

  toggleLoader: Ember.on('init', Ember.observer('isLoadingFiles', 'commonLoader.isLoading', 'commonLoader.type', function() {
    if (this.get('isLoadingFiles')) {
      // prevent loader stealing
      if (!this.get('commonLoader.isLoading')) {
        this.get('commonLoader').setProperties({
          isLoading: true,
          message: this.get('i18n').t('components.dataFilesList.updatingMessage'),
          area: 'content',
          type: 'filesUpdate'
        });
      }
    // prevent closing other types of loader
    } else if (this.get('commonLoader.type') === 'filesUpdate') {
      this.set('commonLoader.isLoading', false);
    }
  })),

  didInsertElement() {
    this.dirChanged();
    if (this.get('uploadEnabled')) {
      console.debug('Binding upload area for file list');
      this.get('fileUpload').assignDrop(this.$());
    }
  },

  dirChanged: function() {
    const dir = this.get('dir');
    this.setProperties({
      'fileUpload.dir': dir,
      'fileBrowser.dir': dir
    });
    this.get('fileSystemTree').expandDir(dir);
  }.observes('dir'),

  fileDownloadServerMethod: Ember.computed('publicMode', function() {
    return this.get('publicMode') ? 'getPublicFileDownloadUrl' : 'getFileDownloadUrl';
  }),

  downloadFile(file, downloadResolve, downloadReject) {
    const i18n = this.get('i18n');
    const messageBox = this.get('messageBox');
    const server = this.get('oneproviderServer');
    const p = server[this.get('fileDownloadServerMethod')](file.get('id'));
    const fileName = (file && file.get('name') || i18n.t('common.unknown'));
    p.then(
      (data) => {
        if (data && data.fileUrl) {
          const iframe = $("<iframe/>").attr({
            src: data.fileUrl,
            style: "visibility:hidden;display:none"
          }).appendTo($('#app'));
          setTimeout(function () {
            iframe.remove();
          }, 1000);
          downloadResolve();
        } else {
          messageBox.open({
            metadata: {name: 'file-download-failure'},
            type: 'error',
            title: i18n.t('components.dataFilesList.downloadError.title'),
            message: i18n.t('components.dataFilesList.downloadError.message', {
              fileName: fileName,
              errorMessage: i18n.t('common.unknownError')
            })
          });
          downloadReject();
        }
      },
      (error) => {
        downloadReject();
        messageBox.open({
          metadata: {name: 'file-download-failure'},
          type: 'error',
          title: i18n.t('components.dataFilesList.downloadError.title'),
          message: i18n.t('components.dataFilesList.downloadError.message', {
            fileName: fileName,
            errorMessage: error && error.message || i18n.t('common.unknownError')
          })
        });
      }
    );

    return p;
  },

  isShowingMetadata: Ember.computed('metadataFile', function() {
    return !!this.get('metadataFile');
  }),

  actions: {
    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    },

    downloadFile(file, resolve, reject) {
      this.downloadFile(file, resolve, reject);
    },

    // TODO: multiple select only with ctrl
    // TODO: select range with shift
    selectFile(file) {
      file.set('isSelected', !file.get('isSelected'));
    },

    openFileShareModal(file) {
      this.sendAction('openFileShareModal', file);
    },

    // TODO: loading
    goUp() {
      this.get('dir.parent').then(
        parentDir => parentDir && this.set('dir', parentDir)
      );
    },

    changeDir(dir) {
      this.set('dir', dir);
    },

    toggleFileMetadata(file) {
      this.get('fileSystemTree').toggleMetadataEditor(file);
    },
  }

});
