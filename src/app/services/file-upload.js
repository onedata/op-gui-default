import Ember from 'ember';
/* globals Resumable */

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

  /** Current dir for upload - global for application! */
  dir: null,

  addFileToUpload(file, parentId) {
    parentId = parentId || this.get('dir.id');
    filesParentDirs[file.uniqueIdentifier] = parentId;
  },

  fileUploadCompleted(file) {
    delete filesParentDirs[file.uniqueIdentifier];
  },

  resumable: function() {
    console.debug(`Creating new Resumable`);
    const r = new Resumable({
      target: '/upload',
      chunkSize: 1*1024*1024,
      simultaneousUploads: 4,
      testChunks: false,
      throttleProgressCallbacks: 1,
      query: (file) => {
        console.debug(`Will upload file ${file} to dir id: ${filesParentDirs[file.uniqueIdentifier]}`);
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
    r.on('fileAdded', (file) => this.addFileToUpload(file));
    r.on('fileSuccess', (file) => this.fileUploadCompleted(file));
    r.on('fileError', (file) => this.fileUploadCompleted(file));

    return r;
  }.property(),

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

  assignBrowse(jqBrowseElement) {
    this.get('resumable').assignBrowse(jqBrowseElement);
  }
});
