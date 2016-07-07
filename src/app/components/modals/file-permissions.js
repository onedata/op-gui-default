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

  isBusy: function() {
    return this.get('isSubmitting') || this.get('isLoadingType');
  }.property('isSubmitting'),

  isSubmitting: false,
  isLoadingType: true,
  error: null,

  file: null,
  acl: null,

  /**
   * Type of permissions to set: "p" for POSIX or "a" for ACL.
   * Set the proper value to switch permissions panel type.
   * It is bound with "permissions type" option on top of modal.
   *
   * PermissionsType cannot be resolved until we are loading ACL
   * (indicated by ``isLoadingType`` attr).
   * @type string
   */
  permissionsType: function() {
    if (this.get('isLoadingType')) {
      return null;
    } else {
      if (this.get('file.permissions') != null) {
        return 'p';
      } else if (this.get('acl')) {
        return 'a';
      } else {
        // TODO: translate
        this.set('error', 'Cannot get neither POSIX permissions nor ACL for file');
        return null;
      }
    }
  }.property('file.permissions', 'file.acl', 'isLoadingType'),

  posixCache: null,
  aclCache: null,

  /**
   * @type File
   */
  model: null,

  getOrCreateAclRecord() {
    const findPromise = this.get('store').find('file-acl', this.get('file.id'));
    findPromise.then(acl => {
      console.debug(`Fetched ACL for file ${this.get('file.id')}`);
      this.set('aclCache', acl);
    });
    findPromise.catch(error => {
      if (error.message === 'No ACL defined.') {
        console.debug(`No ACL found for file ${this.get('file.id')} - new record will be created locally`);
        const newAcl = this.get('store').createRecord('file-acl', {
          file: this.get('file'),
          acl: [ACE.create().toJSON()]
        });
        this.set('aclCache', newAcl);
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

  didInsertElement() {
    this.fileChanged();
  },

  actions: {
    open() {
    },

    opened() {
    },

    submit() {
      this.set('isSubmitting', true);
      this.get('file').save().finally(
        () => {
          this.setProperties({
            isSubmitting: false,
            open: false
          });
        }
      );
    },
  }


});
