import Ember from 'ember';
import PromiseLoadingMixin from '../../mixins/promise-loading';

export default Ember.Component.extend(PromiseLoadingMixin, {
  notify: Ember.inject.service(),
  store: Ember.inject.service(),

  /** @abstract */
  modalId: null,

  /** @abstract */
  label: null,

  open: false,

  file: null,
  acl: null,

  /**
   * Indicates that ``submit`` action is pending.
   */
  isSubmitting: false,

  /**
   * If true, it means that current type of file permission is not resolved.
   * See ``getOrCreateAclRecord`` method.
   */
  isLoadingType: true,
  error: null,

  /**
   * Type of permissions to set: "p" for POSIX or "a" for ACL.
   * Set the proper value to switch permissions panel type.
   * It is bound with "permissions type" option on top of modal.
   *
   * PermissionsType cannot be resolved until we are loading ACL
   * (indicated by ``isLoadingType`` attr).
   * @type string
   */
  permissionsType: null,

  posixCache: null,
  aclCache: null,

  /**
   * @type File
   */
  model: null,

  /**
   * Tries to fetch FileAcl record for currently set file.
   * If ACL exists for file, it sets the ``aclCache`` property and sets
   * permissions type to ACL ('a'), because we want to set a proper view.
   */
  getOrCreateAclRecord() {
    this.setProperties({
      isLoadingType: true,
      permissionsType: null,
    });
    const findPromise = this.get('store').find('file-acl', this.get('file.id'));
    findPromise.then(acl => {
      console.debug(`Fetched ACL for file ${this.get('file.id')}`);
      this.set('aclCache', acl);
      this.set('permissionsType', 'a');
    });
    findPromise.catch(error => {
      if (error.message === 'No ACL defined.') {
        console.debug(`No ACL found for file ${this.get('file.id')} - new record will be created locally`);
        const newAcl = this.get('store').createRecord('file-acl', {
          file: this.get('file'),
          acl: Ember.A()
        });
        this.set('aclCache', newAcl);
        this.set('permissionsType', 'p');
      } else {
        // other errors means that the ACL exists, but it cannot be read
        // so open ACL tab to show a error message to user
        this.set('permissionsType', 'a');
        console.error(`ACL for file ${this.get('file.id')} cannot be fetched due to an error: ${error.message}`);
      }
    });
    findPromise.finally(() => this.set('isLoadingType', false));
  },

  isReadyToSubmit: function() {
    switch (this.get('permissionsType')) {
      case 'p':
        return this.get('posixComponent.isReadyToSubmit');
      case 'a':
        return this.get('aclComponent.isReadyToSubmit');
      default:
        return false;
    }
  }.property(
    'permissionsType',
    'posixComponent.isReadyToSubmit',
    'aclComponent.isReadyToSubmit'
  ).readOnly(),

  fileChanged: function() {
    if (this.get('open') && this.get('file')) {
      this.getOrCreateAclRecord();
    }
  }.observes('file', 'open'),

  actions: {
    open() {
      this.setProperties({
        posixCache: this.get('file.permissions')
      });
      this.fileChanged();
    },

    opened() {
    },

    closed() {
      this.setProperties({
        posixCache: null,
        aclCache: null,
        error: null,
        permissionsType: null,
      });
    },

    submit() {
      this.set('isSubmitting', true);
      const ptype = this.get('permissionsType');
      const aclCache = this.get('aclCache');

      const promises = [];

      if (ptype === 'p') {
        this.set('file.permissions', this.get('posixCache'));
        aclCache.deleteRecord();
        let aclId = aclCache.get('id');
        // ACL is currently saved - so push delete
        if (aclCache.get('id')) {
          let destroyAclPromise = aclCache.save();
          promises.push(destroyAclPromise);
          aclCache.save().catch(
            () => {
              // FIXME: notify error
              console.error(`Removing ACL record ${aclId} failed!`);
            }
          );
        }
      }

      if (ptype === 'a') {
        const aclPromise = this.get('aclCache').save();
        promises.push(aclPromise);
        aclPromise.catch(error => {
          // FIXME: translate
          let msg = `Saving ACL for file ${this.get('aclCache.file.id')} failed: ${error.message}`;
          this.get('notify').error(msg);
          console.error(msg);
        });
      }

      const fileSavePromise = this.get('file').save();
      promises.push(fileSavePromise);

      const completePromise = Ember.RSVP.Promise.all(promises);

      completePromise.then(() => {
        // FIXME: translate
        this.set('open', false);
        this.get('notify').info(`New permissions for file "${this.get('file.name')}" has been set`);
      });

      completePromise.catch(() => {
        // TODO: maybe this should be displayed in modal as an alert panel
        // FIXME: translate
        let msg = `Setting new permissions for file "${this.get('file.name')}" failed!`;
        this.get('notify').error(msg);
        console.error(msg);
      });

      // FIXME: success notify
      // FIXME: do not close window on failure!
      completePromise.finally(() => {
        this.setProperties({
          isSubmitting: false
        });
      });
    },
  }


});
