import Ember from 'ember';

const {
  run,
  computed,
  inject,
  observer,
  RSVP: {
    Promise
  }
} = Ember;

/**
 * Provides set of tools for file manipulation and getting info.
 * Beside toolbar buttons, contains a set of modals for toolbar actions.
 * @module components/data-files-list-toolbar
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  notify: inject.service('notify'),
  fileUpload: inject.service('fileUpload'),
  store: inject.service(),
  fileSystemTree: inject.service(),

  tagName: 'ul',
  classNames: ['data-files-list-toolbar', 'nav', 'navbar-nav', 'navbar-right', 'toolbar-group'],

  fileForChunks: null,
  fileBlocksSorting: ['provider.name'],
  fileBlocksSorted: computed.sort('fileBlocks', 'fileBlocksSorting'),

  chunksModalError: null,

  init() {
    this._super(...arguments);
    this.setProperties({
      isRenamingFile: false,
      isCreatingDir: false,
      isCreatingFile: false,
      isRemovingFiles: false,
      isEditingPermissions: false,
      isFileChunksModal: false,
      isNotImplementedModal: false,
      fileToRename: null,
    });
  },

  selectedCount: computed.alias('dir.selectedFiles.length'),
  
  /**
   * A width of its element. Updated on window resize (see ``didInsertElement``).
   * @type {Number}
   */
  width: undefined,

  // TODO: compute or detect dynamically, when the toolbar should collapse
  /**
   * If true, the toolbar should be collapsed to menu width dropdown.
   * Make full toolbar otherwise.
   * @type {Boolean}
   */
  collapsed: computed('navbarWidth', function() {
    let width = this.get('navbarWidth');
    if (width != null) {
      return width < 880;
    } else {
      return undefined;
    }
  }),

  isSingleFileSelected: computed('dir.singleSelectedFile', function() {
    return this.get('dir.singleSelectedFile') != null;
  }),

  isSingleSelectedFileAFile: computed('isSingleFileSelected', 'dir.singleSelectedFile.isDir', function() {
    return this.get('isSingleFileSelected') && !this.get('dir.singleSelectedFile.isDir');
  }),

  isSomeFileSelected: computed('dir.isSomeFileSelected', function() {
    return this.get('dir.isSomeFileSelected');
  }),

  isMixedTypesSelected: computed('dir.selectedFilesType', function() {
    return this.get('dir.selectedFilesType') === 'mixed';
  }),

  fileUploadLocked: computed.alias('fileUpload.locked'),

  /**
   * Holds items of toolbar. Each item is a Object with properties:
   * - icon {String}
   * - action {String} - name of action to invoke when clicked,
   *     the function should be parameterless
   * - disabled {Boolean}
   * - tooltip {String} - message in tooltip (on hover)
   */
  items: computed(
    'isSingleFileSelected',
    'isSingleSelectedFileAFile',
    'isSomeFileSelected',
    'isMixedTypesSelected',
    'fileUploadLocked',
    
    function() {
      let {
        i18n,
        isSingleFileSelected,
        isSingleSelectedFileAFile,
        isSomeFileSelected,
        isMixedTypesSelected,
        fileUploadLocked
      } = this.getProperties(
        'i18n',
        'isSingleFileSelected',
        'isSingleSelectedFileAFile',
        'isSomeFileSelected',
        'isMixedTypesSelected',
        'fileUploadLocked'
      );

      return [
        {
          id: 'create-dir-tool',
          icon: 'folder-new',
          action: 'createDir',
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.createDir')
        },
        // TODO: temporary, to decide
        {
          id: 'create-file-tool',
          icon: 'file',
          action: 'createFile',
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.createFile')
        },
        {
          id: 'share-file-tool',
          icon: 'share',
          action: 'shareFile',
          disabled: !isSingleFileSelected || isSingleSelectedFileAFile,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.shareFile')
        },
        {
          id: 'file-metadata-tool',
          icon: 'metadata',
          action: 'editFileMetadata',
          disabled: !isSingleFileSelected,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.metadata')
        },
        // using fileUpload service binding
        {
          id: 'upload-file-tool',
          icon: 'upload',
          action: 'uploadBrowse',
          disabled: fileUploadLocked,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.uploadFile')
        },
        {
          id: 'rename-file-tool',
          icon: 'rename',
          action: 'renameSelectedFile',
          disabled: !isSingleFileSelected,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.renameFile')
        },
        {
          id: 'lock-file-tool',
          icon: 'lock',
          action: 'editPermissions',
          disabled: !isSomeFileSelected || isMixedTypesSelected,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.permissions')
        },
        {
          id: 'copy-file-tool',
          icon: 'copy',
          action: 'notImplemented',
          // TODO: feature not implemented yet
          //disabled: !isSomeFileSelected,
          disabled: true,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.copy')
        },
        {
          id: 'cut-file-tool',
          icon: 'cut',
          action: 'notImplemented',
          // TODO: feature not implemented yet
          //disabled: !isSomeFileSelected,
          disabled: true,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.cut')
        },
        {
          id: 'remove-file-tool',
          icon: 'remove',
          action: 'removeSelectedFiles',
          disabled: !isSomeFileSelected,
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.remove')
        },
        {
          id: 'file-chunks-tool',
          icon: 'provider',
          action: 'showChunks',
          disabled: !(isSingleFileSelected && isSingleSelectedFileAFile),
          tooltip: i18n.t('components.dataFilesListToolbar.tooltip.chunks')
        },
      ];
  }),

  fileBlocksProviders: computed('fileBlocks.@each.provider', function() {
    return this.get('fileBlocks').mapBy('provider');
  }),

  makeTooltips: observer('items.[]', 'fileBlocksProviders.@each.name', function() {
    run.scheduleOnce('afterRender', this, function() {
      this.$().find('[data-toggle="tooltip"]').tooltip();
    });
  }),

  didInsertElement() {
    this.makeTooltips();
    console.debug('Binding upload button for files toolbar');
    // NOTE: file upload component has dir set by data-files-list component,
    // so data-files-list _must_ be used when using this toolbar
    // if this changes - please copy "dirChanged" method from files-list here
    this.get('fileUpload').assignBrowse(this.$().find('#toolbar-file-browse'), true);

    run.scheduleOnce('afterRender', this, function() {
      let __windowResizeFun = () => {
        this.set('navbarWidth', this.$().closest('nav.navbar').width());
      };
      __windowResizeFun();
      this.set('__windowResizeFun', __windowResizeFun);
      $(window).on('resize', __windowResizeFun);
    });
  },

  willDestroyElement() {
    $(window).off('resize', this.get('__windowResizeFun'));
  },

  actions: {
    /// Actions on toolbar items click

    renameSelectedFile() {
      if (this.get('dir.singleSelectedFile')) {
        this.setProperties({
          fileToRename: this.get('dir.singleSelectedFile'),
          isRenamingFile: true
        });
      }
    },

    createDir() {
      this.set('createFileName', '');
      this.set('isCreatingDir', true);
    },

    createFile() {
      this.set('createFileName', '');
      this.set('isCreatingFile', true);
    },

    shareFile() {
      this.sendAction('openFileShareModal', this.get('dir.singleSelectedFile'));
    },

    renameModalOpened() {
      this.$().find('input').focus().select();
    },

    // TODO: error handling
    removeSelectedFiles() {
      let selectedToRemoveCount = this.get('selectedCount');
      this.setProperties({
        isRemovingFiles: true,
        selectedToRemoveCount: selectedToRemoveCount
      });
    },

    editPermissions() {
      this.set('newPermissions', '');
      this.set('isEditingPermissions', true);
    },

    showChunks() {
      this.set('isFileChunksModal', true);
      this.set('fileForChunks', this.get('dir.singleSelectedFile'));
      let fileId = this.get('fileForChunks.id');
      // TODO: if fileId null...

      this.get('store').query('file-distribution', { fileId }).then(
        (fbs) => {
          this.set('fileBlocks', fbs);
        },
        (error) => {
          console.error('Error loading file blocks: ' + error.message);
          this.set('chunksModalError', error.message);
        }
      );
    },

    chunksModalClosed() {
      this.setProperties({
        fileForChunks: null,
        fileBlocks: null,
        chunksModalError: null
      });
    },

    editFileMetadata() {
      const file = this.get('dir.singleSelectedFile');
      const fileSystemTree = this.get('fileSystemTree');
      fileSystemTree.toggleMetadataEditor(file);
    },

    uploadBrowse() {
      this.$('#toolbar-file-browse').trigger('click');
    },

    notImplemented() {
      this.set('isNotImplementedModal', true);
    },

    /// Actions for modals
    // TODO: move modals to separate components? (they have some state)

    submitCreateFile(type) {
      this.set('isCreatingFileWorking', true);
      let createPromise = this.get('dir').createFile(type, this.get('createFileName'));
      createPromise.catch((error) => {
        this.get('notify').error(
          this.get('i18n').t('components.dataFilesListToolbar.notify.createFileFailed', {
            fileName: this.get('createFileName')
          }) + ': ' + (error || this.get('i18n').t('common.unknownError'))
        );
      });
      createPromise.finally(() => {
        this.setProperties({
          isCreatingFileWorking: false,
          isCreatingFile: false,
          isCreatingDir: false
        });
      });
    },

    /**
     * Handle Yes/No answer of remove files modal.
     * @param {Boolean} yesAnswer if user answered Yes to remove selected files
     * @param {undefined} _model ignored parameter
     */
    handleRemoveAnswer(yesAnswer, _model, resolve, reject) {
      let { i18n, dir } = this.getProperties('i18n', 'dir');
      let removeResult = dir.removeSelectedFiles();
      
      if (yesAnswer) {
       if (typeof removeResult === 'string') {
          this.set('isRemovingFiles', false);
          reject({message: removeResult});
        } else {
          let notify = this.get('notify');

          let fileDestroyPromises = removeResult;
          let batchPromise = Promise.all(Array.from(fileDestroyPromises.values()));
          let filesCount = fileDestroyPromises.size;
          let singular = fileDestroyPromises.size === 1;
          
          batchPromise.then(() => {
            let message;
            if (singular) {
              let removedFile = fileDestroyPromises.keys().next().value;
              let onlyFileName = removedFile.get('name');
              message = i18n.t('components.dataFilesListToolbar.removeFilesModal.notify.singleRemoveSuccess', {
                fileName: onlyFileName
              });
            } else {
              message = i18n.t('components.dataFilesListToolbar.removeFilesModal.notify.multipleRemoveSuccess', {
                filesCount: filesCount
              });
            }
            notify.info(message);
          });

          batchPromise.catch(() => {
            // some of file destroy promises failed - we need to check async status of all promises
            if (singular) {
              let removedFile = fileDestroyPromises.keys().next().value;
              fileDestroyPromises.values().next().value.catch(error => {
                let onlyFileName = removedFile.get('name');
                let message = i18n.t('components.dataFilesListToolbar.removeFilesModal.notify.singleRemoveFailed', {
                  fileName: onlyFileName,
                  errorMessage: error.message,
                });
                notify.error(message);
              });            
            } else {
              let failCount = 0;
              let successCount = 0;
              let incFailCount = () => failCount += 1;
              let incSuccessCount = () => successCount += 1;
              let checkCompleted = () => {
                if (successCount + failCount === filesCount) {
                  let message;
                  if (successCount > 0) {
                    message = i18n.t('components.dataFilesListToolbar.removeFilesModal.notify.multipleRemoveFailed', {
                      failCount: failCount,
                      filesCount: filesCount,
                    });
                    notify.warning(message);
                  } else {
                    message = i18n.t('components.dataFilesListToolbar.removeFilesModal.notify.allRemoveFailed');
                    notify.error(message); 
                  }
                }
              };
              fileDestroyPromises.forEach((promise/*, file */) => {
                promise.then(incSuccessCount);
                promise.catch(incFailCount);
                promise.finally(checkCompleted);
              });
            }
          });

          batchPromise.finally(() => {
            resolve();
            this.setProperties({
              isRemovingFiles: false,
              selectedToRemoveCount: null,
            });
          });
        }
      } else {
        this.set('isRemovingFiles', false);
      }
    },

    /**
     * Handle result of file rename (action from ``rename-modal``)
     * @param {Boolean} success if rename was done
     * @param {File} model file that has been renamed
     * @param {String} oldName a former name of file to rename
     * @param {BackendError} error has ``message`` property
     */
    renameDone({success, model, oldName, error}) {
      let {i18n, notify} = this.getProperties('i18n', 'notify');
      let file = model;
      let type = i18n.t('common.' + (file.get('isDir') ? 'directory' : 'file'));
      type = String.capitalize(type.toString());
      if (success) {
        notify.info(i18n.t('components.dataFilesListToolbar.renameFileModal.success', {
          type: type,
          oldName: oldName,
          newName: file.get('name')
        }));
      } else {
        notify.error(i18n.t('components.dataFilesListToolbar.renameFileModal.failure', {
          type: type,
          oldName: oldName,
          errorMessage: error.message
        }));
      }
      
    }
  }
});
