import Ember from 'ember';
import PromiseLoadingMixin from '../../mixins/promise-loading';

export default Ember.Component.extend(PromiseLoadingMixin, {
  notify: Ember.inject.service(),

  /** @abstract */
  modalId: null,

  /** @abstract */
  label: null,

  open: false,

  isBusy: function() {
    return this.get('isSubmitting');
  }.property('isSubmitting'),

  isSubmitting: false,

  /**
   * Type of permissions to set: "p" for POSIX or "a" for ACL.
   * Set the proper value to switch permissions panel type.
   * It is bound with "permissions type" option on top of modal.
   * @type string
   */
  permissionsType: 'p',

  /**
   * @type File
   */
  model: null,

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
