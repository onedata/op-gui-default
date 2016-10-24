import Ember from 'ember';
/* globals Resumable */

function matchResumableFileByUuid(rf, resumableFileId) {
  return rf.uniqueIdentifier === resumableFileId;
}

function findResumableFileByUuid(collection, uuid) {
  return collection.find(rf => matchResumableFileByUuid(rf, uuid));
}

/**
 * Enables global usage of file upload.
 * Uses ResumableJS and exposes its object.
 * Exposes jquery assign methods to bind file browser drop and upload button events.
 *
 * ## EventsBus events triggered
 * - file-upload:file-upload-completed(ResumableFile: file, String: parentId)
 * - file-upload:files-added(ResumableFile[]: files, String[]: parentIds)
 *
 * @module services/file-upload
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  component: null,
  session: Ember.inject.service(),
  eventsBus: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),

  /**
   * Current dir for upload - global for application!
   * See a lockedDir property, which is actually used in ResumableJS.
   */
  dir: null,

  /**
   * The lockedDir cannot change if the locked property is true - that means,
   * we are currently adding files to ResumableJS and we use a property
   * to set files parent
   */
  lockedDir: null,

  /**
   * True means that we started adding files to ResumableJS, but we don't
   * finished adding a set of files (the set == files uploaded into single dir).
   * We should not add files to ResumableJS when this flag is true.
   * @private
   */
  locked: false,

  /**
   * Stores list of computed properties names, that store lists for directory uploads.
   * @type {Computed<Ember.Array>}
   */
  dirsUploadIds: Ember.A(),

  /**
   * Add ResumableFile to mapping parentId -> file
   * See also: ``getParentIdOfUploadingFile`` and ``forgetUploadingFile``.
   * @param {ResumableFile} resumableFile
   * @param {String} parentId
   */
  addUploadingFileInfo(resumableFile, parentId) {
    console.debug(`Add uploading file info: ${resumableFile.uniqueIdentifier}`);
    if (this.get('dirUploads-' + parentId) == null) {
      this.get('dirsUploadIds').pushObject(parentId);
      this.set('dirUploads-' + parentId, Ember.A());
      console.debug(`Creating new dirUploads for parent: ${parentId}`);
    }
    let dirUploads = this.get('dirUploads-' + parentId); 
    dirUploads.pushObject(resumableFile);
    console.debug(`dirUploads-${parentId} after add file: ${JSON.stringify(dirUploads.map(f => f.uniqueIdentifier))}`);
  },

  /**
   * Provides global file.uniqueIdentifier -> parentDir.id, to get upload mapping.
   * @param {String} resumableFileId
   * @return {String} parentId
   */
  getParentIdOfUploadingFile(resumableFileId) {
    let dirsUploadIds = this.get('dirsUploadIds');

    for (let parentId of dirsUploadIds) {
      let dirUploads = this.get('dirUploads-' + parentId);
      let resumableFile = findResumableFileByUuid(dirUploads, resumableFileId);
      if (resumableFile) {
        return parentId;
      }
    }

    return null;
  },

  /**
   * Remove entry of ResumableFile from parent -> uploading files mapping.
   * @param {String} resumableFileId
   * @return {[String, Number]} [parentId of removed ResumableFile entry,
   *                            number of remain files in parent]
   */
  forgetUploadingFile(resumableFileId) {
    console.debug(`Forgetting uploaded file: ${resumableFileId}`); 
    let dirsUploadIds = this.get('dirsUploadIds');
    for (let parentId of dirsUploadIds) {
      /* jshint loopfunc: true */
      let propertyKey = 'dirUploads-' + parentId;
      let dirUploads = this.get(propertyKey);
      console.debug(`Current uploading files for parent ${parentId}: ${JSON.stringify(dirUploads.toArray().map(f => f.uniqueIdentifier))}`);
      let resumableFile = findResumableFileByUuid(dirUploads, resumableFileId);
      if (resumableFile) {
        Ember.assert(
          'ResumableFile not removed from dirUploads',
          dirUploads.removeObject(resumableFile)
        );
        let remainUploadingFilesCount = dirUploads.get('length');
        // FIXME cleaning up old dirUploads-... properties
        if (remainUploadingFilesCount === 0) {
          console.debug(`Removing uploading dir info: ${parentId}`);
          this.set(propertyKey, undefined);
          this.get('dirsUploadIds').removeObject(parentId);
        } else {
          console.debug(`Remain uploading files for parent ${parentId}: ${JSON.stringify(dirUploads.toArray().map(f => f.uniqueIdentifier))}`);
        }
        return [parentId, remainUploadingFilesCount];
      }
    }
    console.warn(`Tried to remove info about resumable file upload:
${resumableFileId}, but it could not be found in any dir`);
    return [null, null];
  },

  /**
   * The lockedDir property should be an alias for dir, but we do not want
   * to alter the lockedDir when fileUpload is in locked=true state.
   * So we do not use a standard Ember.property.alias method but instead
   * we observe both dir and lockedDir properties for change - if we are not
   * "locked" we can set lockedDir to dir value.
   */
  dirChanged: function() {
    if (!this.get('locked')) {
      this.set('lockedDir', this.get('dir'));
      console.debug(`locked dir changed: ${this.get('lockedDir.id')}`);
    }
  }.observes('dir', 'locked'),

  /**
   * Handles fileaAdded event of ResumableJS. Adds an entry to file -> parentDir
   * map and sets a service into locked state (see "locked" property).
   */
  fileAdded(file, parentId) {
    if (!this.get('locked')) {
      // Ember.run is used because this fun is invoked from ResumableJS event
      Ember.run(() => this.set('locked', true));
    }
    Ember.run(() => {
      parentId = parentId || this.get('lockedDir.id');
      this.addUploadingFileInfo(file, parentId);
    });
  },

  fileUploadSuccess(file) {
    this.get('oneproviderServer').fileUploadSuccess(
      file.uniqueIdentifier,
      this.getParentIdOfUploadingFile(file.uniqueIdentifier)
    );
    this.fileUploadCompleted(file);
  },

  fileUploadFailure(file) {
    this.get('oneproviderServer').fileUploadFailure(
      file.uniqueIdentifier,
      this.getParentIdOfUploadingFile(file.uniqueIdentifier)
    );
    this.fileUploadCompleted(file);
  },

  /**
   * Handles the ResumableJS file upload finish events (when it succeeded or failed)
   */
  fileUploadCompleted(file) {
    let uuid = file.uniqueIdentifier;
    console.debug(`File upload completed: ${uuid}`); 
    setTimeout(() => {
      let [parentId, filesLeft] = this.forgetUploadingFile(uuid);
      this.get('eventsBus').trigger(
        'file-upload:file-upload-completed',
        file,
        parentId
      );
      if (filesLeft <= 0) {
        this.onAllFilesForDirUploaded(parentId);
      }
    }, 1000);
    
  },

  /**
   * Handler for filesAdded event of ResumableJS.
   * Ivoking means that we finished adding files for single directory, so
   * we can "unlock" the service (see "locked" property).
   */
  filesAdded(files) {
    // Ember.run is used because this fun is invoked from ResumableJS event
    Ember.run(() => {
      this.set('locked', false);
      this.get('eventsBus').trigger('file-upload:files-added', files,
        files.map(f => this.getParentIdOfUploadingFile[f.uniqueIdentifier])
      );
    });
  },

  onAllFilesForDirUploaded(dirId) {
    console.debug(`Finished batch upload for dir: ${dirId}`);
    if (!dirId) {
      console.warn('dirId in batch upload complete RPC is null - do not make RPC call');
    } else {
      let rpc = this.get('oneproviderServer').fileBatchUploadComplete(dirId);
          rpc.catch(error => {
            console.error(`fileBatchUploadComplete RPC failed: ${error.message},
Directory content won't be updated!`);
          });
    }
  },

  resumable: function() {
    console.debug(`Creating new Resumable`);
    const r = new Resumable({
      target: '/upload',
      chunkSize: 1*1024*1024,
      simultaneousUploads: 4,
      testChunks: false,
      throttleProgressCallbacks: 1,
      permanentErrors: [400, 404, 405, 415, 500, 501],
      query: (file) => {
        return {
          parentId: this.getParentIdOfUploadingFile(file.uniqueIdentifier)
        };
      },
      generateUniqueIdentifier: function() {
        let date = new Date().getTime();
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g,
          function (character) {
            let random = (date + Math.random() * 16) % 16 | 0;
            date = Math.floor(date / 16);
            return (character === 'x' ? random : (random & 0x7 | 0x8)).toString(16);
          });
      }
    });

    // event handlers mainly to prevent changing parent directory adding files to upload
    r.on('fileAdded', (file) => this.fileAdded(file));
    r.on('filesAdded', (files) => this.filesAdded(files));
    r.on('fileSuccess', (file) => this.fileUploadSuccess(file));
    r.on('fileError', (file) => this.fileUploadFailure(file));

    return r;
  }.property(),

  /**
   * Pass a jQuery element to make it a drop area for files uploading.
   * For more information, see ResumableJS docs.
   */
  assignDrop(jqDropElement) {
    this.get('resumable').assignDrop(jqDropElement);

    let lastEnter;

    let startDrag = function(event) {
      lastEnter = event.target;
      jqDropElement.addClass('file-drag');
    };

    let endDrag = function(event) {
      if (lastEnter === event.target) {
        jqDropElement.removeClass('file-drag');
      }
    };

    jqDropElement.on('dragenter', startDrag);
    jqDropElement.on('dragleave', endDrag);
    jqDropElement.on('dragend', endDrag);
    jqDropElement.on('drop',  endDrag);
  },

  /**
   * Pass a jQuery element to make it a button for files uploading.
   * For more information, see ResumableJS docs.
   */
  assignBrowse(jqBrowseElement) {
    this.get('resumable').assignBrowse(jqBrowseElement);
  }
});
