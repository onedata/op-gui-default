import Ember from 'ember';

const {
  computed
} = Ember;

const UploadingFile = Ember.Object.extend({
  uuid: computed('resumableFile', function() {
    return this.get('resumableFile').uniqueIdentifier;
  }),
  resumableFile: null,

  /**
   * Should be updated on resumable js progress events MANUALLY.
   * Range: 0..1
   * @type {Number}
   */
  progress: null,
  completed: computed('progress', function() {
    return this.get('progress') >= 1;
  }),
  error: null,

  init() {
    this._super(...arguments);
    let rfile = this.get('resumableFile');
    Ember.assert(
      'resumableFile injected to UploadingFile cannot be null',
      rfile
    );
    this.setProperties({
      progress: rfile.progress(),
    });
  }
});

const HIDE_AFTER_COMPLETE_TIMEOUT_MS = 2000;

/**
 * A file upload status container. When no file is uploaded, it is hidden.
 * It requires a file-upload-service to run, which hosts a ResumableJS instance.
 * @module components/file-upload
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  fileUploadService: Ember.inject.service('file-upload'),
  notify: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  session: Ember.inject.service(),

  classNames: ['file-upload'],
  classNameBindings: ['visible:file-upload-visible:file-upload-hidden'],

  uploadAddress: '/upload',

  /**
   * If true, the panel is shown.
   * @type {Boolean}
   */
  visible: false,

  /**
   * @private
   * @type {UploadingFile[]}
   */
  uploadingFiles: Ember.A(),

  /**
   * Range: 0..1
   * @type {Nubmer}
   */
  progress: 0,

   /**
   * @private
   * @type {UploadingFile[]}
   */
  uploadingFilesNotStarted: computed('uploadingFiles.@each.progress', function() {
    return this.get('uploadingFiles').filter((ufile) => {
      let progress = ufile.get('progress');
      return progress <= 0;
    });
  }),

  /**
   * @private
   * @type {UploadingFile[]}
   */
  uploadingFilesInProgress: computed('uploadingFiles.@each.progress', function() {
    return this.get('uploadingFiles').filter((ufile) => {
      let progress = ufile.get('progress');
      return progress > 0 && progress < 1;
    });
  }),

  /**
   * @private
   * @type {UploadingFile[]}
   */
  uploadingFilesDone: computed('uploadingFiles.@each.{error,completed}', function() {
    return this.get('uploadingFiles').filter((ufile) => {
      return ufile.get('completed') && !ufile.get('error');
    });
  }),

    /**
   * @private
   * @type {UploadingFile[]}
   */
  uploadingFilesFailed: computed('uploadingFiles.@each.error', function() {
    return this.get('uploadingFiles').filter((ufile) => {
      return ufile.get('error') != null;
    });
  }),

  init() {
    this._super(...arguments);
    this._resetProperties();
  },

  clearFiles() {
    this.set('uploadingFiles', Ember.A());
  },

  _resetProperties() {
    this.setProperties({
      uploadingFiles: Ember.A(),
      visible: false,
    });
  },

  resumable: function() {
    return this.get('fileUploadService.resumable');
  }.property('fileUploadService.resumable'),

  getUploadingFile(resumableFile) {
    let ufiles = this.get('uploadingFiles');
    return ufiles.filter(uf => uf.get('uuid') === resumableFile.uniqueIdentifier).get('firstObject');
  },

  addUploadingFile(resumableFile) {
    let uploadingFiles = this.get('uploadingFiles');
    console.debug('components/file-upload: File added: ' + resumableFile.fileName);
    let ufile = UploadingFile.create({
      resumableFile: resumableFile
    });
    uploadingFiles.pushObject(ufile);
    return ufile;
  },

  /**
   * Adds instance of UploadingFile corresponding to given resumableFile.
   * @param {ResumableFile} resumableFile
   * @returns {UploadingFile} uploading file that is in uploadingFiles
   *                          (added or found exisiting)
   */
  addOrGetUploadingFile(resumableFile) {
    let ufile = this.getUploadingFile(resumableFile);
    if (!ufile) {
      ufile = this.addUploadingFile(resumableFile);
    }
    return ufile;
  },

  /**
   * @type {Function}
   */
  onFileAdded: computed(function() {
    return (file) => {
      this.set('visible', true);
      // Add the file to the list
      this.addOrGetUploadingFile(file);
    };
  }),

  // TODO: pausing support
  // onPause: computed(function() {
  //   return function() {
  //     // TODO: Show resume, hide pause
  //   };
  // }),

  onComplete: computed(function() {
    return () => {
      // TODO: Hide pause/resume when the upload has completed
      // FIXME: experimental one notify
      let notify = this.get('notify');
      let filesCount = this.get('uploadingFiles.length');
      let failedFilesCount = this.get('uploadingFilesFailed.length');
      if (failedFilesCount === 0) {
        notify.info(`Completed upload of ${filesCount} file(s)`);
      } else if (failedFilesCount < filesCount) {
        notify.warning(`${failedFilesCount} of ${filesCount} file(s) cannot be uploaded`);
      } else {
        notify.error(`Files upload failed!`);
      }
      
      // FIXME: experimental - close upload component after some time
      // FIXME: make class with close/show animation?
      setTimeout(() => {
        this.set('visible', false);
        this.clearFiles();
        this.set('progress', 0);
        // FIXME: reset of ResumableJS must be done to reset progress!
        // this.get('fileUploadService').resetResumableInstance();
      }, HIDE_AFTER_COMPLETE_TIMEOUT_MS);
    };
  }),

  onFileSuccess: computed(function() {
    return (/*file, message*/) => {
      // FIXME: make one notify after batch files upload
      // FIXME: i18n
      // this.get('notify').info(`File "${file.fileName}" uploaded successfully!`);
    };
  }),

  onFileError: computed(function() {
    return (file, message) => {
      let ufile = this.addOrGetUploadingFile(file);
      ufile.set('error', message || "unknown error");
    };
  }),

  percentageProgress: computed('progress', function() {
    let p = this.get('progress');
    return Math.floor(p*100 || 0);
  }),

  // TODO: make progress bar component
  progressBarStyle: computed('percentageProgress', function() {
    let pp = this.get('percentageProgress');
    Ember.assert(
      'file-upload percentage progress should be between 0..100',
      pp >= 0 && pp <= 100
    );
    return Ember.String.htmlSafe(
      `width:${pp}%;`
    );
  }),

  onFileProgress: computed(function() {
    let r = this.get('resumable');
    return (file) => {
      // Handle progress for both the file and the overall upload
      let ufile = this.addOrGetUploadingFile(file);
      ufile.set('progress', file.progress());
      this.set('progress', r.progress());
    };
  }),

  // TODO: cancel support
  // onCancel: function() {
  //   return function() {
  //     $('.resumable-file-progress').html('canceled');
  //   };
  // }.property(),

  // TODO: start/pause support
  // onUploadStart: computed(function() {
  //   return () => {
  //     // Show pause, hide resume
  //     $('.resumable-progress .progress-resume-link').hide();
  //     $('.resumable-progress .progress-pause-link').show();
  //   };
  // }),

  didInsertElement() {
    let r = this.get('fileUploadService.resumable');

    // TODO: use component selector this.$().find(...)

    if (!r.support) {
      // TODO: transalte or other message, this should be blocking error
      this.get('notify').error('ResumableJS is not supported in this browser!');
    }

    // FIXME: getProperties instead of each get
    r.on('fileAdded', this.get('onFileAdded'));
    // r.on('pause', this.get('onPause'));
    r.on('complete', this.get('onComplete'));
    r.on('fileSuccess', this.get('onFileSuccess'));
    r.on('fileError', this.get('onFileError'));
    r.on('fileProgress', this.get('onFileProgress'));
    // r.on('cancel', this.get('onCancel'));
    // r.on('uploadStart', this.get('onUploadStart'));
  },

  registerComponentInService: function() {
    this.set('fileUploadService.component', this);
  }.on('init'),
});
