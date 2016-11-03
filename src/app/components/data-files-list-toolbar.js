import Ember from 'ember';

/**
 * Provides set of tools for file manipulation and getting info.
 * Beside toolbar buttons, contains a set of modals for toolbar actions.
 * @module components/data-files-list-toolbar
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  notify: Ember.inject.service('notify'),
  fileUpload: Ember.inject.service('fileUpload'),
  store: Ember.inject.service(),
  fileSystemTree: Ember.inject.service(),

  tagName: 'ul',
  classNames: ['nav', 'navbar-nav', 'navbar-right', 'toolbar-group'],

  fileForChunks: null,
  fileBlocksSorting: ['provider.name'],
  fileBlocksSorted: Ember.computed.sort('fileBlocks', 'fileBlocksSorting'),

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
    });
  },

  selectedCount: Ember.computed.alias('dir.selectedFiles.length'),

  /**
   * Holds items of toolbar. Each item is a Object with properties:
   * - icon {String}
   * - action {String} - name of action to invoke when clicked,
   *     the function should be parameterless
   * - disabled {Boolean}
   * - tooltip {String} - message in tooltip (on hover)
   */
  items: function() {
    const i18n = this.get('i18n');
    const isSingleFileSelected = this.get('dir.singleSelectedFile');
    const isSingleSelectedFileAFile = isSingleFileSelected && !this.get('dir.singleSelectedFile.isDir');
    const isSomeFileSelected = this.get('dir.isSomeFileSelected');
    const isMixedTypesSelected = (this.get('dir.selectedFilesType') === 'mixed');
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
        disabled: this.get('fileUpload.locked'),
        tooltip: i18n.t('components.dataFilesListToolbar.tooltip.uploadFile')
      },
      {
        id: 'rename-file-tool',
        icon: 'rename',
        action: 'renameSelectedFile',
        // TODO: feature not implemented yet
        //disabled: !isSingleFileSelected,
        disabled: true,
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
  }.property('dir.isSomeFileSelected', 'dir.singleSelectedFile', 'fileUpload.locked'),

  fileBlocksProviders: Ember.computed('fileBlocks.@each.provider', function() {
    return this.get('fileBlocks').mapBy('provider');
  }),

  makeTooltips: function() {
    Ember.run.scheduleOnce('afterRender', this, function() {
      this.$().find('[data-toggle="tooltip"]').tooltip();
    });
  }.observes('items', 'fileBlocksProviders.@each.name'),

  didInsertElement() {
    this.makeTooltips();
    console.debug('Binding upload button for files toolbar');
    // NOTE: file upload component has dir set by data-files-list component,
    // so data-files-list _must_ be used when using this toolbar
    // if this changes - please copy "dirChanged" method from files-list here
    this.get('fileUpload').assignBrowse(this.$().find('#toolbar-file-browse'), true);
  },

  actions: {
    /// Actions on toolbar items click

    renameSelectedFile() {
      if (this.get('dir.singleSelectedFile')) {
        this.set('renameFileName', '');
        this.set('isRenamingFile', true);
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
      // TODO: should use autofocus of modal bs-form-element, but it does not work
      // $('*').focus(function(event) {
      //   debugger;
      // });

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

      this.get('store').query('file-distribution', { filter: { fileId: fileId } }).then(
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

    submitRenameSelectedFile() {
      try {
        let file = this.get('dir.singleSelectedFile');
        if (file) {
          if (this.get('renameFileName')) {
            file.set('name', this.get('renameFileName') || '');
            file.save();
          } else {
            console.error('Please enter non-blank file name');
          }
        } else {
          console.error('No file selected to rename or multiple selected');
        }
      } finally {
        this.set('isRenamingFile', false);
      }
    },

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
      let removeResult = this.get('dir').removeSelectedFiles();
      let i18n = this.get('i18n');

      if (yesAnswer) {
       if (typeof removeResult === 'string') {
          this.set('isRemovingFiles', false);
          reject({message: removeResult});
        } else {
          let fileDestroyPromises = removeResult;
          let batchPromise = Ember.RSVP.Promise.all(Array.from(fileDestroyPromises.values()));
          let filesCount = fileDestroyPromises.size;
          let singular = fileDestroyPromises.size === 1;
          let notify = this.get('notify');

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
  }
});
