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

  /**
   * To inject. Optional.
   *
   * If true, files list will have a file drop area to upload files.
   * @type {Boolean}
   * @default true
   */
  uploadEnabled: true,

  /**
   * To inject. Optional.
   *
   * If true, a breadcrumbs component will be shown on top of file browser.
   * It allows to naviage through dirs tree of the list.
   * @type {Boolean}
   * @default false
   */
  breadcrumbsEnabled: false,

  /**
   * To inject.
   * One of: data, shared, public
   * @type {String}
   */
  downloadMode: false,

  /**
   * To inject.
   * Optional: if specified, breadcrumbs will have this dir as a root.
   * Otherwise, breadcrumbs will display full parents path.
   * @type {File}
   */
  rootDir: null,

  /**
   * To inject.
   * If true, content cannot be edited.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  /**
   * A parent directory to list its files
   * @type {File}
   */
  dir: null,

  /**
   * If scrolling, how many files ahead we should invoke more files loading.
   * @type {Number}
   */
  preloadAheadIndexes: 10,

  /**
   * True, if fetch more files has been requested but not completed.
   * @type {Boolean} 
   */
  fetchMoreFilesRequested: false,

  isLoadingMoreFiles: Ember.computed.alias('fetchMoreFilesRequested'),

  /**
   * Promise create when requested more files from backend.
   * Not null if at least one request for fetching more files was made.
   * @type {RSVP.Promise}
   */
  fetchMoreFilesPromise: null,

  /**
   * If fetching more files ended with an error, store it here.
   * If it is not null, fetching more files will be disabled.
   */
  fetchMoreFilesError: null,

  /**
   * True if all children files of the ``dir`` are loaded (using backend paging).
   * @type {Boolean}
   */
  allFilesLoaded: false,

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
   * It is used to toggle a loader indicator for files browser.
   */
  isLoadingFiles: Ember.computed('files.isUpdating', 'files', 'files.[]', 'files.@each.isLoaded', 'fileUpload.locked', function() {
    return this.get('fileUpload.locked') ||
      !!this.get('files').any((f) => !f.get('isLoaded')) ||
      this.get('files.isUpdating');
  }),

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

  /**
   * Which file with index should be watched for visibility.
   * @type {Number}
   */
  indexToWatch: Ember.computed('visibleFiles.length', 'preloadAheadIndexes', function() {
    return this.get('visibleFiles.length') - this.get('preloadAheadIndexes') - 1;
  }),

  didInsertElement() {
    this.dirChanged();
    if (this.get('uploadEnabled')) {
      console.debug('Binding upload area for file list');
      this.get('fileUpload').assignDrop(this.$());
    }
  },

  resetProperties() {
    this.setProperties({
      fetchMoreFilesRequested: false,
      fetchMoreFilesError: null,
      fetchMoreFilesPromise: null,
      allFilesLoaded: false,
    });
  },

  dirChanged: Ember.observer('dir', function() {
    const dir = this.get('dir');
    this.setProperties({
      'fileUpload.dir': dir,
      'fileBrowser.dir': dir
    });
    this.get('fileSystemTree').expandDir(dir);
    this.resetProperties();
    this.set('allFilesLoaded', this.get('dir.allChildrenLoaded') === true);
  }),

  fileDownloadServerMethod: Ember.computed('downloadMode', function() {
    switch (this.get('downloadMode')) {
      case 'data':
        return 'getFileDownloadUrl';
      case 'shared':
        return 'getSharedFileDownloadUrl';
      case 'public':
        return 'getPublicFileDownloadUrl';
      default:
        return 'getFileDownloadUrl';
    }
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

    fetchMoreFiles(resolve, reject) {
      if (!this.get('fetchMoreFilesRequested')) {
        const currentFilesCount = this.get('files.length');
        this.set('fetchMoreFilesRequested', true);
        try {
          const fetchPromise = this.get('oneproviderServer').fetchMoreDirChildren(this.get('dir.id'), this.get('files.length'));
          fetchPromise.then((data) => {
            const filesCount = data.newChildrenCount;
            if (filesCount <= currentFilesCount) {
              this.set('allFilesLoaded', true);
            }
          });
          fetchPromise.catch((error) => {
            this.set('fetchMoreFilesError', error);
            this.get('notify').error(this.get('i18n').t('components.dataFilesList.cannotFetchMoreFiles', {
              errorMessage: error.message
            }));
          });
          fetchPromise.finally(() => this.set('fetchMoreFilesRequested', false));
          this.set('fetchMoreFilesPromise', fetchPromise);
        } catch (error) {
          this.set('fetchMoreFilesRequested', false);
          throw error;
        }
      } else {
        console.debug('Requested fetching more files, but already waiting...');
      }

      // get previously saved promise - this can be promise created in this or previous invocation
      const promise = this.get('fetchMoreFilesPromise');
      // handle resolve/reject passed to this action
      promise.then((data) => {
        if (resolve) {
          resolve(data);
        }
      },
      (error) => {
        if (reject) {
          reject(error);
        }
      });
    },

    fileAppeared(/*index*/) {
      const props = this.getProperties(
        'allFilesLoaded',
        'fetchMoreFilesRequested',
        'fetchMoreFilesError',
        'isLoadingFiles'
      );
      if (!props.isLoadingFiles &&
        !props.allFilesLoaded &&
        !props.fetchMoreFilesRequested &&
        !props.fetchMoreFilesError) {
          this.send('fetchMoreFiles');
      }
    }
  }

});
