import Ember from 'ember';
/* globals Resumable */

/**
 * A global file.uniqueIdentifier -> parentDir.id, to remember upload mapping.
 */
const filesParentDirs = {};

/**
 * Enables global usage of file upload.
 * Uses ResumableJS and exposes its object.
 * Exposes jquery assign methods to bind file browser drop and upload button events.
 * @module services/file-upload
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  component: null,
  session: Ember.inject.service(),

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
   */
  locked: false,

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
   * Handles fileaAdded even of ResumableJS. Adds an entry to file -> parentDir
   * map and sets a service into locked state (see "locked" property).
   */
  fileAdded(file, parentId) {
    parentId = parentId || this.get('lockedDir.id');
    filesParentDirs[file.uniqueIdentifier] = parentId;
    if (!this.get('locked')) {
      // Ember.run is used because this fun is invoked from ResumableJS event
      Ember.run(() => this.set('locked', true));
    }
  },

  /**
   * Handles the ResumableJS file upload finish events (when it succeeded or failed)
   */
  fileUploadCompleted(file) {
    delete filesParentDirs[file.uniqueIdentifier];
  },

  /**
   * Handler for filesAdded event of ResumableJS.
   * Ivoking means that we finished adding files for single directory, so
   * we can "unlock" the service (see "locked" property).
   */
  filesAdded(/*files*/) {
    // Ember.run is used because this fun is invoked from ResumableJS event
    Ember.run(() => this.set('locked', false));
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
          parentId: filesParentDirs[file.uniqueIdentifier],
          connectionRef: this.get('session.sessionDetails.connectionRef')
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
    r.on('fileSuccess', (file) => this.fileUploadCompleted(file));
    r.on('fileError', (file) => this.fileUploadCompleted(file));

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
