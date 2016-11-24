import Ember from 'ember';

/**
 * Files and subdirectories browser for single directory.
 * For file operations, see ``data-files-list-toolbar`` component.
 *
 * ## Loading states
 * Files list shows various loading indicators:
 * - global loader
 *   - blocks usage of whole data files list (currently blocks
 *      whole content)
 *   - it is turned on when loading children list of injected dir for
 *     the first time
 *   - see ``showGlobalLoader`` and ``toggleLoader`` method
 * - more files requested loader
 *   - is shown on the bottom of files list, can be not visible for user when
 *     he scrolls the list
 *   - is turned on when we reached some point in files list and we are making
 *     request to fetch more files and new portion of files is not pushed yet
 *   - see ``isLoadingMoreFiles`` property
 * - last files push collection loading
 *   - blocks usage of files fetched after last files push before all files
 *     from this push are loaded
 *   - see ``data-files-list-loader`` component, which renders the loader
 *   - see ``loadingFileIndex`` for row from what the loader begins on top
 * - waiting for new files list push after upload finished
 *   - see ``isWaitingForPushAfterUpload`` property and its setting and getting
 * 
 * ## EventsBus events emitted
 * - dataFilesList:dirChanged({dir: File})
 * 
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
  eventsBus: Ember.inject.service(),

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
   * Where the browser is used.
   * Possible values: data, shared, public
   * @type {String}
   */
  browserLocation: 'data',

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
   * To inject.
   * A parent directory to list its files
   * @type {File}
   */
  dir: null,

  /**
   * To inject. Optional.
   * If scrolling, how many files ahead we should invoke more files loading.
   * @type {Number}
   */
  preloadAheadIndexes: 10,

  /// State of component

  /**
   * True, if fetch more files has been requested but not completed.
   * @private
   * @type {Boolean}
   */
  fetchMoreFilesRequested: false,

  /**
   * Promise create when requested more files from backend.
   * Not null if at least one request for fetching more files was made.
   * @private
   * @type {RSVP.Promise}
   */
  fetchMoreFilesPromise: null,

  /**
   * If fetching more files ended with an error, store it here.
   * If it is not null, fetching more files will be disabled.
   * @private
   * @type {Object} errorResponse has ``.message`` String property
   */
  fetchMoreFilesError: null,

  /**
   * How many files are ready for user to use. This does not include files
   * that are fetched with last ``getMoreFiles`` and not all files from this
   * collection are loaded.
   * @private
   * @type {Number}
   */
  readyFilesCount: 0,

  /**
   * True if all children from injected ``dir`` were loaded.
   * This is changed only one time - ignores ``dir.children`` (files) collection
   * changes.
   * @private
   * @type {Boolean}
   */
  firstLoadDone: false,

  init() {
    this._super(...arguments);
    this.resetProperties();
  },

  /**
   * After invoking ``fetchMoreFiles`` we know how many files this list
   * will contain after new model push. This variable contains this count.
   * This variable is set before a model push is done.
   * @private
   * @type {Computed<Number>}
   */
  totalAheadFilesCount: 0,

  /**
   * Set to true, if finished upload but not yet received push with new files list.
   * @private
   */
  isWaitingForPushAfterUpload: false,

  /**
   * True if all children files of the ``dir`` are loaded (using backend paging).
   * @type {Computed<Boolean>}
   */
  areAllFilesLoaded: Ember.computed.alias('dir.allChildrenLoaded'),

  /**
   * One of: data, shared, public
   * @type {Computed<String>}
   */
  downloadMode: Ember.computed.alias('browserLocation'),

  /**
   * True, if fetch more files has been requested but not completed.
   * @private
   * @type {Boolean}
   */
  isLoadingMoreFiles: Ember.computed('fetchMoreFilesRequested', 'isPushAfterFetchMoreDone', function() {
    return this.get('fetchMoreFilesRequested') || !this.get('isPushAfterFetchMoreDone');
  }),

  /**
   * A type of used model of ``dir`` deduced from used file brower mode.
   * @type {Computed<String>}
   */
  fileModelType: Ember.computed('downloadMode', function() {
    switch (this.get('downloadMode')) {
      case 'data':
        return 'file';
      case 'shared':
        return 'file-shared';
      case 'public':
        return 'file-public';
      default:
        return undefined;
    }
  }),

  /**
   * An index of file row, which is a first of rows collection that
   * is loading in latest ``fetchMoreFiles`` operation.
   * The index is returned only if we are loading files.
   * It is used to render a ``data-files-list-loader`` overlay on "loading" file rows.
   * @type {Number}
   */
  loadingFileIndex: Ember.computed('areLastRequestedFilesLoaded', 'isFilesLoading', 'readyFilesCount', function() {
    if (this.get('isFilesLoading') || !this.get('areLastRequestedFilesLoaded')) {
      return this.get('readyFilesCount');
    }
  }),

  /**
   * True if last requested more files are fetched and available for user.
   * It means that user does not requested more files to fetch, so we does not
   * need any loader.
   * @type {Computed<Boolean>}
   */
  areLastRequestedFilesLoaded: Ember.computed('totalAheadFilesCount', 'loadedFiles.length', function() {
    return this.get('totalAheadFilesCount') <= this.get('loadedFiles.length');
  }),

  isPushAfterFetchMoreDone: Ember.computed('totalAheadFilesCount', 'files.length', function() {
    return this.get('totalAheadFilesCount') <= (this.get('files.length') || 0);
  }),

  /**
   * Collection of dir children. These are not files that are displayed.
   * See ``visibleFiles``.
   * @type {Computed<File[]>}
   */
  files: Ember.computed.alias('dir.children'),

  /**
   * Filtered collection of ``files`` that are loaded.
   * @type {Computed<File[]>}
   */
  loadedFiles: Ember.computed('files.@each.isLoaded', function() {
    return this.get('files').filter(f => f.get('isLoaded'));
  }),

  /**
   * Collection of files that are displayed in files browser.
   * For order of display see ``visibleFilesSorted``.
   * @type {Computed<File[]>}
   */
  visibleFiles: Ember.computed('loadedFiles', function() {
    return this.get('loadedFiles').filter(f => !f.get('isBroken'));
  }),

  /**
   * List of files that are displayed in files browser.
   * Currently files are sorted by backend and we do not change this.
   * @type {Computed<File[]>}
   */
  visibleFilesSorted: Ember.computed.alias('visibleFiles'),

  selectedFiles: Ember.computed.filterBy('visibleFiles', 'isSelected', true),

  /**
   * True if there is nothing to display in files browser.
   * However, there can be some children files, but they cannot be displayed.
   * @type {Computed<Boolean>}
   */
  dirIsEmpty: Ember.computed('visibleFiles.length', function() {
    let dirIsEmpty = !this.get('visibleFiles') || this.get('visibleFiles.length') === 0;
    return dirIsEmpty;
  }),

  filesTableIsVisible: Ember.computed('dirIsEmpty', 'currentlyUploadingCount', function() {
    let props = this.getProperties('dirIsEmpty', 'currentlyUploadingCount');
    let filesTableIsVisible = !props.dirIsEmpty || props.currentlyUploadingCount;
    if (filesTableIsVisible) {
      Ember.run.scheduleOnce('afterRender', this, function() {
        this.computeFileLabelMaxWidth();
      });
    }
    return filesTableIsVisible;
  }),

  /**
   * Checks if each in ``files`` collection is ready to read.
   * @type {Computed<Booblean>}
   */
  isFilesLoading: Ember.computed('files.isUpdating', 'files.@each.isLoaded', function() {
    return this.get('files.isUpdating') || this.get('files').any(f => !f.get('isLoaded'));
  }),

  /**
   * True if a "global loader" that blocks all
   * @type {Computed<Boolean>}
   */
  showGlobalLoader: Ember.computed('firstLoadDone', 'isFilesLoading', 'isLoadingMoreFiles', function() {
    let props = this.getProperties(
      'firstLoadDone',
      'isFilesLoading',
      'isLoadingMoreFiles'
    );

    if (!props.firstLoadDone && !props.isLoadingMoreFiles &&
      (props.isFilesLoading)) {

      this.set('firstLoadDone', true);
      return true;
    } else {
      return false;
    }
  }),

  // TODO: there is an assumption that the push brings at least one visible file
  // if there will be problems with push, please fix this by watching if push failed
  filesListChanged: Ember.observer('visibleFiles.length', function() {
    let visibleFilesLength = this.get('visibleFiles.length');
    let __prevVisibleFilesLength = this.get('__prevVisibleFilesLength');
    if (!__prevVisibleFilesLength || __prevVisibleFilesLength < visibleFilesLength) {
      // files count have been increased, so any wait for upload results should finish 
      this.set('isWaitingForPushAfterUpload', false); 
    }
    
    this.set('__prevVisibleFilesLength', visibleFilesLength);
  }),

  /**
   * When files push come, increase number of files in ``totalAheadFilesCount``
   */
  updateTotalAheadFilesCount: Ember.observer('files.length', function() {
    let filesLength = this.get('files.length');
    let totalAheadFilesCount = this.get('totalAheadFilesCount');
    let fetchMoreFilesRequested = this.get('fetchMoreFilesRequested');
    if (fetchMoreFilesRequested && filesLength > totalAheadFilesCount) {
      console.debug(`Total ahead files count updated because files.length increased`);
      this.set('totalAheadFilesCount', filesLength);
    }
  }),

  toggleLoader: Ember.on('init', Ember.observer('showGlobalLoader', 'commonLoader.isLoading', 'commonLoader.type', function() {
    if (this.get('showGlobalLoader')) {
      // prevent loader stealing
      if (!this.get('commonLoader.isLoading')) {
        let area;
        switch (this.get('browserLocation')) {
          case 'data':
            area = 'content-with-secondary-top';
            break;
          case 'public':
            area = 'public-content';
            break;
          default:
            area = 'content';
            break;
        }

        this.get('commonLoader').setProperties({
          isLoading: true,
          solidBackground: true,
          message: this.get('i18n').t('components.dataFilesList.updatingMessage'),
          area: area,
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
  fileIndexToWatchVisibility: Ember.computed('visibleFiles.length', 'preloadAheadIndexes', function() {
    let index = this.get('visibleFiles.length') - this.get('preloadAheadIndexes') - 1;
    return index > 0 ? index : 0;
  }),

  // FIXME
  // visibleFilesCount: Ember.computed({
  //   get() {
  //     this.get('__visibleFilesCount');
  //   },
  //   set(key, value) {
  //     this.setProperties({
  //       __prevVisibleFilesCount: this.get('__visibleFilesCount'),
  //       __visibleFilesCount: value
  //     });
  //     return value;
  //   }
  // }),

  currentlyUploadingCount: Ember.computed({
    get() {
      this.get('__currentlyUploadingCount');
    },
    set(key, value) {
      this.setProperties({
        __prevCurrentlyUploadingCount: this.get('__currentlyUploadingCount'),
        __currentlyUploadingCount: value
      });
      return value;
    }
  }),

  // TODO VFS-2753: don't know if this code works
  currentlyUploadingCountChanged: Ember.observer('currentlyUploadingCount', function() {
    let {count, __prevCurrentlyUploadingCount} =
      this.getProperties('currentlyUploadingCount', '__prevCurrentlyUploadingCount');
    if (!count && __prevCurrentlyUploadingCount) {
      console.debug(`Batch upload finished for ${this.get('dir.name')}`);
      this.set('isWaitingForPushAfterUpload', true);
    }
  }),

  computeFileLabelMaxWidth() {
    let firstColumn = this.$().find('table > thead > th:nth-child(1)');
    let fileLabelMaxWidth = firstColumn.width() - 70;
    fileLabelMaxWidth = fileLabelMaxWidth < 0 ? 0 : fileLabelMaxWidth;
    this.set(
      'fileLabelMaxWidth',
      fileLabelMaxWidth
    );
  },

  // FIXME: on init, we should read current dirUploads from service
  // and reset currentlyUploadingCount property value
  handleDirUploadsChanged({parentId, dirUploads}) {
    if (this.get('dir.id') === parentId) {
      this.set('currentlyUploadingCount', dirUploads.get('length')); 
    }
  },

  didInsertElement() {
    let eventsBus = this.get('eventsBus');

    let __computeFileLabelMaxWidth = this.computeFileLabelMaxWidth.bind(this);
    eventsBus.on('secondarySidebar:resized', this, 'computeFileLabelMaxWidth');
    $(window).on('resize.dataFilesList', __computeFileLabelMaxWidth);
    setTimeout(__computeFileLabelMaxWidth, 50);

    eventsBus.on('fileUpload:dirUploadsChanged', this, 'handleDirUploadsChanged');

    this.dirChanged();
    if (this.get('uploadEnabled')) {
      console.debug('Binding upload area for files list');
      this.get('fileUpload').assignDrop(this.$());
    }
  },

  willDestroyElement() {
    this.setGlobalDir(null);

    let ebus = this.get('eventsBus');

    ebus.off('secondarySidebar:resized', this, 'computeFileLabelMaxWidth');
    ebus.off('fileUpload:dirUploadsChanged', this, 'handleDirUploadsChanged');
    
    $(window).off('.dataFilesList');
  },

  /**
   * Sets global file browser state.
   * @param {File} dir
   */
  setGlobalDir(dir) {
    this.setProperties({
      'fileUpload.dir': dir,
      'fileBrowser.dir': dir
    });
  },

  /**
   * Set default values for state properties.
   */
  resetProperties() {
    this.setProperties({
      fetchMoreFilesRequested: false,
      fetchMoreFilesPromise: null,
      fetchMoreFilesError: null,
      readyFilesCount: 0,
      totalAheadFilesCount: 0,
      firstLoadDone: false,
    });
  },

  dirChanged: Ember.observer('dir', function() {
    this.set('currentlyUploadingCount', 0);
    let {dir, eventsBus} = this.getProperties('dir', 'eventsBus');
    this.setGlobalDir(dir);
    this.get('fileSystemTree').expandDir(dir);
    this.resetProperties();

    eventsBus.trigger('dataFilesList:dirChanged', {dir: dir});
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

  clearFilesSelection() {
    this.get('files').forEach(f => f.set('isSelected', false));
  },

  findNearestSelectedIndex(fileIndex) {
    let {visibleFilesSorted, selectedFiles} =
      this.getProperties('visibleFilesSorted', 'selectedFiles');

    // [index: Number, distanceFromFile: Number]
    let selectedFilesIndexes = selectedFiles.map(sf => {
      let index = visibleFilesSorted.indexOf(sf);
      return [index, Math.abs(index-fileIndex)];
    });
    let nearest = selectedFilesIndexes.reduce((prev, current) => {
      return current[1] < prev[1] ? current : prev;
    }, [-1, Infinity]);
    let [nearestIndex, nearestDist] = nearest;
    if (nearestDist === Infinity) {
      nearestIndex = fileIndex;
    }
    return nearestIndex;
  },

  /**
   * Select files range using shift.
   * Use nearest selected file as range start.
   * @param {File} file
   */
  selectRangeToFile(file) {
    let {visibleFilesSorted, lastSelectedFile} =
      this.getProperties('visibleFilesSorted', 'lastSelectedFile');
    let fileIndex = visibleFilesSorted.indexOf(file);

    let startIndex;
    if (lastSelectedFile) {
      startIndex = visibleFilesSorted.indexOf(lastSelectedFile);
    } else {
      startIndex = this.findNearestSelectedIndex(fileIndex);
    }   

    let indexA = Math.min(startIndex, fileIndex);
    let indexB = Math.max(startIndex, fileIndex);
    visibleFilesSorted.slice(indexA, indexB+1).forEach(f => f.set('isSelected', true));
  },

  actions: {
    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    },

    downloadFile(file, resolve, reject) {
      this.downloadFile(file, resolve, reject);
    },

    // TODO: select range with shift
    /**
     * Do something if user clicks on a file. Consider modifier keys
     */
    handleFileClicked(file, ctrlKey, shiftKey) {
      let files = this.get('files');
      let fileIsSelected = file.get('isSelected');
      let otherFilesSelected = files.filter(f => {
        return f !== file && f.get('isSelected');
      }).length > 0;
      if (otherFilesSelected) {
        if (fileIsSelected) {
          if (ctrlKey) {
            file.set('isSelected', false);
            this.set('lastSelectedFile', null);
          } else {
            this.clearFilesSelection();
            file.set('isSelected', true);
            this.set('lastSelectedFile', file);
          }
        } else {
          if (ctrlKey) {
            file.set('isSelected', true);
            this.set('lastSelectedFile', file);
          } else {
            if (shiftKey) {
              this.selectRangeToFile(file);
            } else {
              this.clearFilesSelection();
              file.set('isSelected', true);
              this.set('lastSelectedFile', file);
            }
          }
        }
      } else {
        if (fileIsSelected) {
          file.set('isSelected', false);
          this.set('lastSelectedFile', null);
        } else {
          file.set('isSelected', true);
          this.set('lastSelectedFile', file);
        }
      }
    },

    clearFilesSelection() {
      this.clearFilesSelection();
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
      this.sendAction('openDirInBrowser', dir);
    },

    toggleFileMetadata(file) {
      this.get('fileSystemTree').toggleMetadataEditor(file);
    },

    fetchMoreFiles(resolve, reject) {
      if (!this.get('fetchMoreFilesRequested')) {
        const readyFilesCount = this.get('files.length');
        this.set('readyFilesCount', readyFilesCount);
        this.set('fetchMoreFilesRequested', true);
        try {
          const fetchPromise = this.get('oneproviderServer')
            .fetchMoreDirChildren(
              this.get('dir.id'),
              this.get('files.length'),
              this.get('fileModelType')
            );
          fetchPromise.then(data => {
            console.debug('Fetch more files promise resolved, new count: ' + data.newChildrenCount);
            this.set('totalAheadFilesCount', data.newChildrenCount);
          });
          fetchPromise.catch(error => {
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

    /**
     * Handle an action when file notified that became visible for user on the list.
     * In some circumstances, we want then ``fetchMoreFiles``.
     */
    fileAppeared(/*index*/) {
      const props = this.getProperties(
        'areAllFilesLoaded',
        'fetchMoreFilesRequested',
        'fetchMoreFilesError',
        'isFilesLoading'
      );
      if (!props.isFilesLoading &&
        !props.areAllFilesLoaded &&
        !props.fetchMoreFilesRequested &&
        !props.fetchMoreFilesError) {
          this.send('fetchMoreFiles');
      }
    }
  }
});
