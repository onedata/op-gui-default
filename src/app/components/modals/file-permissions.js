import Ember from 'ember';
import PromiseLoadingMixin from '../../mixins/promise-loading';
import ACE from '../../utils/access-control-entity';

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
   * If true, all controls inside modal should be disabled.
   * It indicates, that some action is pending and it's dangerous to make other actions.
   */
  isBusy: function() {
    return this.get('isSubmitting') || this.get('isLoadingType');
  }.property('isSubmitting', 'isLoadingType'),

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
          acl: Ember.A([ACE.create()])
        });
        this.set('aclCache', newAcl);
        this.set('permissionsType', 'p');
      } else {
        console.debug(`ACL for file ${this.get('file.id')} cannot be fetched due to an error: ${error.message}`);
        // FIXME: lock possibility to edit ACL with message
      }
    });
    findPromise.finally(() => this.set('isLoadingType', false));
  },

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
          aclCache.save().then(
            () => {
            },
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
          console.error(`Saving ACL for file ${this.get('aclCache.file.id')} failed: ${error.message}`);
          // FIXME: error notify
        });
      }

      const fileSavePromise = this.get('file').save();
      promises.push(fileSavePromise);

      const completePromise = Ember.RSVP.Promise.all(promises);

      completePromise.then(() => {
        this.get('notify').info(`New permissions for file "${this.get('file.name')}" has been set`);
      });

      completePromise.catch(() => {
        this.get('notify').error(`Setting new permissions for file "${this.get('file.name')}" failed!`);
      });

      // FIXME: success notify
      // FIXME: do not close window on failure!
      completePromise.finally(() => {
        this.setProperties({
          isSubmitting: false,
          open: false
        });
      });
    },
  }


});
