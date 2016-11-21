import Ember from 'ember';
import PromiseLoadingMixin from 'op-worker-gui/mixins/promise-loading';
import { mergeAcls } from 'op-worker-gui/utils/acl-utils';
import { POSIX_SPECIAL_DIFFERENT } from 'op-worker-gui/components/file-permissions/posix';

const PT_ACL = 'a';
const PT_POSIX = 'p';
const PT_MIXED = 'm';
const PT_DENIED = 'd';
const PT_ERROR = 'e';

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
  filesToPermissions: null,

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
   * Type of permissions to set: 
   * * "p" for POSIX
   * * "a" for ACL.
   * * "m" for "mixed" - we have mixed POSIX/ACL permissions for selected ``files``.
   * * "d" for access denied for at least one permission
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
      filesToPermissions: new Map(),
    });
  },

  unloadPermissions() {
    this.get('filesToPermissions').forEach(perm => {
      if (perm) {
        perm.unloadRecord();
      }
    });
  },

  /**
   * Tries to fetch FileAcl record for currently set file.
   * If ACL exists for file, it sets the ``aclCache`` property and sets
   * permissions type to ACL (PT_ACL), because we want to set a proper view.
   */
  fetchFilePermissionRecord(file) {
    return new Ember.RSVP.Promise((resolve, reject) => {
      const findPromise = file.get('filePermission');
      findPromise.then(fileAcl => resolve(fileAcl));
      findPromise.catch(error => reject(error));
    });
  },

  isReadyToSubmit: function() {
    if (this.get('statusBlocked')) {
      return false;
    } else {
      switch (this.get('permissionsType')) {
        case PT_POSIX:
          // TODO: data down, actions up (send notify from posix component)
          return this.get('posixComponent.isReadyToSubmit');
        case PT_ACL:
          // TODO: data down, actions up (send notify from acl component)
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
    let perms = Array.from(this.get('filesToPermissions').values());
    let posixes = perms.map(perm => perm.get('posixValue'));

    debugger;

    let allPosixesAreSame = posixes.every(p => p === posixes[0]);
    if (allPosixesAreSame) {
      this.set('posixCache', posixes[0]);
    } else {
      // magic code - this means, that not all posixes are same
      this.set('posixCache', POSIX_SPECIAL_DIFFERENT);
    }
  },

  /**
   * Fetches FileAcl records for all ``files`` and sets ``filesToPermissions`` map.
   * Resolved array could have null elements - it indicates, that some files
   * have no ACL at all.
   * @returns {Ember.RSVP.Promise<FileAcl[]>} promise resolving with FileAcl[]
   *                                          (each fileAcl for files)
   */
  fetchAllFilePermissionRecords() {
    const filePermissionsPromises = [];
    this.get('files').forEach(f => {
      let promise = this.fetchFilePermissionRecord(f);
      filePermissionsPromises.push(promise);
      promise.then(perm => {
        this.get('filesToPermissions').set(f, perm);
      });
    });

    return Ember.RSVP.all(filePermissionsPromises);
  },

  /**
   *  @param {(FilePermissions[])} filePermissions
   *    result array from ``fetchAllFilePermissionsRecords`` promise resolve
   */
  handleResolvedPermissions(filePermissions) {
    let atLeastOneAccessDenied =
      filePermissions.any(a => a.get('type') === 'eaccess');
    if (atLeastOneAccessDenied) {
      this.set('permissionsType', PT_DENIED);
    } else {
      let atLeastOneWithoutAcl = filePermissions.any(a => a.get('type') !== 'acl');
      if (atLeastOneWithoutAcl) {
        let noFileWithAcl = filePermissions.every(a => a.get('type') !== 'acl');
        if (noFileWithAcl) {
          // there is no ACL at all, set editor mode to POSIX
          this.set('permissionsType', PT_POSIX);
        } else {
          // some files have POSIX and some have ACLs
          this.set('permissionsType', PT_MIXED);
        }
      } else {
        // every file has a non-empty ACL
        this.set('permissionsType', PT_ACL);
      }

      // create aclCache from available ACLs to show something in ACL editor
      let acls = filePermissions.map(perm => perm ? perm.get('aclValue') : null);
       let mergedAcls = mergeAcls(acls);

      // not very efficient...
      let mixedAcl = mergedAcls && mergedAcls.length > 0 &&
        (JSON.stringify(mergedAcls) !== JSON.stringify(acls[0]));

      this.setProperties({
        aclCache: mergedAcls,
        mixedAcl: mixedAcl
      });
    }
  },

  handlePermissionsTypeChange: Ember.observer('permissionsType', 'mixedLock', function() {
    let {permissionsType, i18n} = this.getProperties('permissionsType', 'i18n');

    if (permissionsType !== PT_MIXED) {
      // changing permissionsType from mixed to posix/acl
      this.setProperties({
        statusMeta: null,
        statusBlocked: false,
        statusType: null,
        statusMessage: null
      });
    }

    switch (permissionsType) {
      case PT_MIXED:
        this.setProperties({
          statusMeta: 'mixedLock',
          statusBlocked: true,
          statusType: 'warning',
          statusMessage: i18n.t('components.modals.filePermissions.mixedPermissionsMessage')
        });
        break;
      case PT_DENIED:
        this.setProperties({
          statusMeta: 'aclEaccess',
          statusBlocked: true,
          statusType: 'warning',
          statusMessage: i18n.t('components.modals.filePermissions.aclEaccessMessage')
        });
        break;
      case PT_ERROR:
        this.setProperties({
          statusMeta: 'permissionsError',
          statusBlocked: true,
          statusType: 'warning',
          statusMessage: i18n.t('components.modals.filePermissions.errorMessage')
        });
        break;
      default:
        break;
    }
  }),

  /**
   * There is at least one ACL
   * and it cannot be fetched because of permission denied etc.
   *
   * **Sets values of ``permissionsType`` property**
   */
  handleRejectedPermissions(error) {
    console.warn(`components/file-permissions: Some FileAcl records cannot be fetched by current user: ${error.message}`);
    this.setProperties({
      permissionsType: PT_ERROR,
      error: error.message
    });
  },

  /**
   * This function sets ``isLoadingType`` to true when working
   */
  updatePermissionsCache: Ember.observer('open', 'files.@each.id', function() {
    this.setProperties({
      isLoadingType: true,
      permissionsType: null,
    });

    let {files, open} = this.getProperties('files', 'open');
    if (open && files) {
      this.fetchAllFilePermissionRecords()
        .then(perms => this.handleResolvedPermissions(perms))
        .catch(error => this.handleRejectedPermissions(error))
        .finally(() => {
          this.set('isLoadingType', false);
        });
    }
  }),

  /**
   * Uses ``posixCache`` (POSIX permissions in editor) to set POSIX permissions
   * in all filePermissions.
   */
  submitPosix() {
    let {posixCache, filesToPermissions} = this.getProperties(
      'posixCache', 'filesToPermissions'
    );

    let promises = [];

    // all files ACL's should be removed
    filesToPermissions.forEach(permission => {
      if (permission) {
        // each file's permissions should be set to one stored in posixCache
        permission.setProperties({
          type: 'posix',
          posixValue: posixCache,
        });
        promises.push(permission.save());
      }
    });

    return Ember.RSVP.all(promises);
  },

  /**
   * Uses ``aclCache`` (ACL permissions in editor) to create/modify FileAcl
   * records for all files in ``filesToPermissions`` map.
   * It doesn't modify POSIX permissions for files.
   */
  submitAcl() {
    let {aclCache, filesToPermissions} = this.getProperties('aclCache', 'filesToPermissions');

    const savePromises = [];

    // set the edited ACL to all ACLs (for each file)
    filesToPermissions.forEach(fileAcl => {
      fileAcl.setProperties({
        type: 'acl',
        aclValue: aclCache
      });

      savePromises.push(fileAcl.save());
      // FIXME: handle reject - rollback attributes
    });

    return Ember.RSVP.all(savePromises);
  },

  actions: {
    open() {
    },

    opened() {
    },

    closed() {
      this.unloadPermissions();
      this.resetProperties();
    },

    /**
     * Saves permissions changes in all files.
     * It uses ``this.submitPosix()`` or ``this.submitAcl()``
     * basing on ``permissionsType`` (PT_POSIX or PT_ACL).
     *
     * It binds a generic submission handlers on specific submit promises.
     */
    submit() {
      this.set('isSubmitting', true);

      let submitPromise;
      switch (this.get('permissionsType')) {
        case PT_POSIX:
          submitPromise = this.submitPosix();
          break;
        case PT_ACL:
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
