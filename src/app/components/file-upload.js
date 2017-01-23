import Ember from 'ember';

const {
  computed,
  run,
  assert,
  inject,
  A,
  String: {
    htmlSafe
  }
} = Ember;

/**
 * Ember Class for storing "model" of file that is for upload.
 * Each ``UploadingFile`` shoud have reference to corresponding ``ResumableFile``
 * that is used by ResumableJS. ``UploadingFile`` instances are used widely in
 * file-upload component (code and template).
 * ``UploadingFile`` instances are rendered with ``uploading-file`` components.
 */
const UploadingFile = Ember.Object.extend({
  uuid: computed('resumableFile', function() {
    return this.get('resumableFile').uniqueIdentifier;
  }),
  resumableFile: null,

  completed: computed('progress', function() {
    return this.get('progress') >= 1;
  }),
  error: null,

  init() {
    this._super(...arguments);
    let rfile = this.get('resumableFile');
    assert(
      'resumableFile injected to UploadingFile cannot be null',
      rfile
    );
    this.setProperties({
      progress: rfile.progress(),
    });
  }
});

const CLEAR_AFTER_COMPLETE_TIMEOUT_MS = 250;

/**
 * A file upload status container. When no file is uploaded, it is hidden.
 * It requires a file-upload-service to run, which hosts a ResumableJS instance.
 * @module components/file-upload
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  fileUploadService: inject.service('file-upload'),
  notify: inject.service(),
  oneproviderServer: inject.service(),
  session: inject.service(),

  classNames: ['file-upload'],
  classNameBindings: ['isPanelVisible:file-upload-visible:file-upload-hidden'],

  uploadAddress: '/upload',

  /**
   * @private
   * @type {UploadingFile[]}
   */
  uploadingFiles: A(),

  /**
   * If true, panel is visible
   * @type {Boolean}
   */
  isPanelVisible: false,

  progress: computed('visible', 'uploadingFiles.@each.progress', function() {
    let r = this.get('resumable');
    let progress = r.progress();
    // uncomment for verbose:
    // console.debug(`components/file-upload progress changed to: ${progress}`);
    return progress;
  }),

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
    this.set('uploadingFiles', A());
  },

  _resetProperties() {
    this.setProperties({
      uploadingFiles: A(),
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
    let uploadingFilesCount = this.get('uploadingFiles.length');
    if (uploadingFilesCount >= 0) {
      this.set('isPanelVisible', true);
    }
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
      this.addOrGetUploadingFile(file);
    };
  }),

  onComplete: computed(function() {
    return () => {
      // TODO: i18n
      // TODO: Hide pause/resume when the upload has completed
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
      
      let props = this.getProperties('uploadingFilesDone', 'uploadingFilesFailed');
      let finishedUploadingFiles = props.uploadingFilesDone.concat(props.uploadingFilesFailed);
      let finishedResumableFiles = finishedUploadingFiles.map(uf => uf.get('resumableFile'));

      this.set('isPanelVisible', false);

      setTimeout(() => {
        let uploadingFiles = this.get('uploadingFiles');
        let resumable = this.get('resumable');
        this.set(
          'uploadingFiles',
          uploadingFiles.filter(uf => !finishedUploadingFiles.includes(uf))
        );
        resumable.files = resumable.files.filter(rf => !finishedResumableFiles.includes(rf));
        // HACK: forcing ResumableJS to forget last progress - not very safe, but should work
        resumable._prevProgress = 0;
      }, CLEAR_AFTER_COMPLETE_TIMEOUT_MS);
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
    assert(
      'file-upload percentage progress should be between 0..100',
      pp >= 0 && pp <= 100
    );
    return htmlSafe(
      `width:${pp}%;`
    );
  }),

  onFileProgress: computed(function() {
    return (file) => {
      // Handle progress for both the file and the overall upload
      let ufile = this.addOrGetUploadingFile(file);
      ufile.set('progress', file.progress());
    };
  }),

  didInsertElement() {
    let r = this.get('fileUploadService.resumable');

    // TODO: use component selector this.$().find(...)

    if (!r.support) {
      // TODO: transalte or other message, this should be blocking error
      this.get('notify').error('ResumableJS is not supported in this browser!');
    }

    let callbacks = this.getProperties(
      'onFileAdded',
      'onComplete',
      'onFileError',
      'onFileProgress'
    );

    // bind some callbacks for ResumableJS events
    // events not bound: onPause, onFileSuccess, onCancel, onUploadStart
    run.scheduleOnce('afterRender', this, function() {
      r.on('fileAdded', callbacks.onFileAdded);
      r.on('complete', callbacks.onComplete);
      r.on('fileError', callbacks.onFileError);
      r.on('fileProgress', callbacks.onFileProgress);
    });
  },

  registerComponentInService: function() {
    this.set('fileUploadService.component', this);
  }.on('init'),
});
