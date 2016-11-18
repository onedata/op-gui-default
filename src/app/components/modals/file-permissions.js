import Ember from 'ember';
import PromiseLoadingMixin from 'op-worker-gui/mixins/promise-loading';
import { mergeAcls } from 'op-worker-gui/utils/acl-utils';
import { POSIX_SPECIAL_DIFFERENT } from 'op-worker-gui/components/file-permissions/posix';

export default Ember.Component.extend(PromiseLoadingMixin, {
  notify: Ember.inject.service(),
  store: Ember.inject.service(),
  i18n: Ember.inject.service(),

  /** @abstract */
  modalId: null,

  /** @abstract */
  label: null,

  open: false,

  files: null,

  /**
   * Maps ``File`` -> ``FileAcl``
   * FileAcl value can be null if the file currently does not have ACL.
   * It will be created on submit with ACL set on.
   * @type Map
   */
  filesToFileAcl: null,

  /**
   * Indicates that ``submit`` action is pending.
   */
  isSubmitting: false,

  /**
   * If true, it means that current type of file permission is not resolved.
   * See ``fetchFileAclRecord`` method.
   */
  isLoadingType: true,
  error: null,

  /**
   * Type of permissions to set: "p" for POSIX or "a" for ACL.
   * "m" for "mixed" - we have mixed POSIX/ACL permissions for selected ``files``.
   * Set the proper value to switch permissions panel type.
   * It is bound with "permissions type" option on top of modal.
   *
   * PermissionsType cannot be resolved until we are loading ACL
   * (indicated by ``isLoadingType`` attr).
   * @type string
   */
  permissionsType: null,

  /**
   * Currently edited POSIX permissions codes - can be saved to all selected files on submit.
   * @type Number
   */
  posixCache: null,

  /**
   * Currently edited ACL - it can be saved to ACLs of all selected files on submit.
   * @type AccessControlEntity[]
   */
  aclCache: null,

  /**
   * @type File
   */
  model: null,

  /**
   * If true, ACL was constructed from mixed content.
   */
  mixedAcl: false,

  init() {
    this._super();
    this.resetProperties();
  },

  resetProperties() {
    this.setProperties({
      posixCache: null,
      aclCache: null,
      error: null,
      permissionsType: null,
      filesToFileAcl: new Map(),
    });
  },

  /**
   * Tries to fetch FileAcl record for currently set file.
   * If ACL exists for file, it sets the ``aclCache`` property and sets
   * permissions type to ACL ('a'), because we want to set a proper view.
   */
  fetchFileAclRecord(file) {
    return new Ember.RSVP.Promise((resolve, reject) => {
      const findPromise = file.get('fileAcl');
      findPromise.then(fileAcl => {
        resolve(fileAcl);
      });
      findPromise.catch(error => {
        reject(error);
      });
    });
  },

  isReadyToSubmit: function() {
    if (this.get('statusBlocked')) {
      return false;
    } else {
      switch (this.get('permissionsType')) {
        case 'p':
          return this.get('posixComponent.isReadyToSubmit');
        case 'a':
          return this.get('aclComponent.isReadyToSubmit');
        default:
          return false;
      }
    }
  }.property(
    'error',
    'permissionsType',
    'posixComponent.isReadyToSubmit',
    'aclComponent.isReadyToSubmit'
  ).readOnly(),

  filesPermissionsChanged: Ember.observer('open', 'files.@each.permissions', function() {
    if (this.get('open')) {
      this.updatePosixCache();
    }
  }),

  updatePosixCache() {
    const posixes = this.get('files').map(f => f.get('permissions'));

    const allPosixesAreSame = posixes.every(p => p === posixes[0]);
    if (allPosixesAreSame) {
      this.set('posixCache', posixes[0]);
    } else {
      // magic code - this means, that not all posixes are same
      this.set('posixCache', POSIX_SPECIAL_DIFFERENT);
    }
  },

  /**
   * Fetches FileAcl records for all ``files`` and sets ``filesToFileAcl`` map.
   * Resolved array could have null elements - it indicates, that some files
   * have no ACL at all.
   * @returns {Ember.RSVP.Promise<FileAcl[]>} promise resolving with FileAcl[]
   *                                          (each fileAcl for files)
   */
  fetchAllFileAclRecords() {
    const fileAclPromises = [];
    this.get('files').forEach(f => {
      let promise = this.fetchFileAclRecord(f);
      fileAclPromises.push(promise);
      promise.then(acl => {
        this.get('filesToFileAcl').set(f, acl);
      });
    });

    return Ember.RSVP.all(fileAclPromises);
  },

  /**
   *  @param {(FileAcl[])} fileAcls
   *    result array from ``fetchAllFileAclRecords`` promise resolve;
   *    elements can be null!
   */
  handleResolvedAcls(fileAcls) {
    const atLeastOneWithoutAcl = fileAcls.any(a => a.get('notExists'));
    if (atLeastOneWithoutAcl) {
      const noFileWithAcl = fileAcls.every(a => a.get('notExists'));
      if (noFileWithAcl) {
        // there is no ACL at all, set editor mode to POSIX
        this.set('permissionsType', 'p');
      } else {
        // some files have POSIX and some have ACLs
        this.set('permissionsType', 'm');
      }
    } else {
      // every file has a non-empty ACL
      this.set('permissionsType', 'a');
    }

    // create aclCache from available ACLs to show something in ACL editor
    const acls = fileAcls.map(fa => fa ? fa.get('acl') : null);
    const mergedAcls = mergeAcls(acls);

    // not very efficient...
    const mixedAcl = mergedAcls && mergedAcls.length > 0 &&
      (JSON.stringify(mergedAcls) !== JSON.stringify(acls[0]));

    this.setProperties({
      aclCache: mergedAcls,
      mixedAcl: mixedAcl
    });
  },

  handlePermissionsTypeChange: Ember.observer('permissionsType', 'mixedLock', function() {
    let {permissionsType} = this.getProperties('permissionsType');

    if (permissionsType !== 'm') {
      // changing permissionsType from mixed to posix/acl
      this.setProperties({
        statusMeta: null,
        statusBlocked: false,
        statusType: null,
        statusMessage: null
      });
    }

    if (permissionsType === 'm') {
      this.setProperties({
        statusMeta: 'mixedLock',
        statusBlocked: true,
        statusType: 'warning',
        statusMessage: this.get('i18n').t('components.modals.filePermissions.mixedPermissionsMessage')
      });
    } else if (permissionsType === 'a') {
      let acls = Array.from(this.get('filesToFileAcl').values());

      if (acls.some(a => a.get('accessDenied'))) {
        this.setProperties({
          statusMeta: 'aclEaccess',
          statusBlocked: true,
          statusType: 'warning',
          statusMessage: this.get('i18n').t('components.modals.filePermissions.aclEaccessMessage')
        });
      }
    }
  }),

  /**
   * There is at least one ACL
   * and it cannot be fetched because of permission denied etc.
   *
   * **Sets values of ``permissionsType`` property**
   */
  handleRejectedAcls(error) {
    console.debug(`components/file-permissions: Some FileAcl records cannot be fetched by current user: ${error.message}`);
    this.set('permissionsType', 'a');
  },

  /**
   * Sets new content of ``aclCache``.
   * **This function sets ``isLoadingType`` to true when working**
   *
   *
   */
  updateAclCache: Ember.observer('open', 'files.@each.id', function() {
    this.setProperties({
      isLoadingType: true,
      permissionsType: null,
    });

    const ps = this.getProperties('files', 'open');
    if (ps.open && ps.files) {
      this.fetchAllFileAclRecords()
        // all ACLs can be fetched or currently don't exist
        .then((acls) => this.handleResolvedAcls(acls))
        // there is at least one ACL that cannot be fetched
        .catch(error => this.handleRejectedAcls(error))
        .finally(() => {
          this.set('isLoadingType', false);
        });
    }
  }),

  /**
   * Uses ``posixCache`` (POSIX permissions in editor) to set POSIX permissions
   * in all files in ``files`` property.
   * Destroys all FileAcl records associated with files (``filesToFileAcl`` map)
   */
  submitPosix() {
    let {posixCache, filesToFileAcl} = this.getProperties(
      'posixCache', 'filesToFileAcl'
    );

    let promises = [];

    // all files ACL's should be removed
    filesToFileAcl.forEach((fileAcl, file) => {
      if (fileAcl && fileAcl.get('status') !== 'ne') {
        // each file's permissions should be set to one stored in posixCache
        file.set('permissions', posixCache);
        if (file.get('hasDirtyAttributes')) {
          promises.push(file.save());
        }
        // only set file.fileAcl to "not exists" status, do not destroy it
        fileAcl.setProperties({
          status: 'ne',
          acl: null,
        });
        promises.push(fileAcl.save());
      }
    });

    return Ember.RSVP.all(promises);
  },

  /**
   * Uses ``aclCache`` (ACL permissions in editor) to create/modify FileAcl
   * records for all files in ``filesToFileAcl`` map.
   * It doesn't modify POSIX permissions for files.
   */
  submitAcl() {
    const aclCache = this.get('aclCache');
    const filesToFileAcl = this.get('filesToFileAcl');

    const savePromises = [];

    // set the edited ACL to all ACLs (for each file)
    filesToFileAcl.forEach((fileAcl, file) => {
      if (fileAcl.get('notExists')) {
        fileAcl.setProperties({
          status: 'ok',
          file: file,
          acl: aclCache
        });
      } else {
        // this file had an ACL before - update its "acl" list to the edited one
        fileAcl.set('acl', aclCache);
      }

      savePromises.push(fileAcl.save());
    });

    return Ember.RSVP.all(savePromises);
  },

  actions: {
    open() {
    },

    opened() {
    },

    closed() {
      this.resetProperties();
    },

    /**
     * Saves permissions changes in all files.
     * It uses ``this.submitPosix()`` or ``this.submitAcl()``
     * basing on ``permissionsType`` ('p' or 'a').
     *
     * It binds a generic submission handlers on specific submit promises.
     */
    submit() {
      this.set('isSubmitting', true);

      let submitPromise;
      switch (this.get('permissionsType')) {
        case 'p':
          submitPromise = this.submitPosix();
          break;
        case 'a':
          submitPromise = this.submitAcl();
          break;
      }

      submitPromise
        .then(() => this.submitSucceed())
        .catch(() => this.submitFailed())
        .finally(() => this.submitCompleted());
    },

  },

  submitSucceed() {
    this.set('open', false);
    const msg = this.get('i18n').t('components.modals.filePermissions.submitSuccess');
    this.get('notify').info(msg);
  },

  submitFailed() {
    // TODO: maybe this should be displayed in modal as an alert panel
    const msg = this.get('i18n').t('components.modals.filePermissions.submitFailed');
    this.get('notify').error(msg);
  },

  submitCompleted() {
    this.set('isSubmitting', false);
  }

});
