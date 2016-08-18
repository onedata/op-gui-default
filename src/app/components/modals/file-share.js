import Ember from 'ember';
import PromiseLoadingMixin from '../../mixins/promise-loading';

const DEFAULT_SHARE_TYPE =  'snapshot';
const DEFAULT_SHARE_PUBLIC_ACCESS = false;

/**
 * Modal that allows to create a Share from selected directory.
 *
 * @module modals/file-share
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(PromiseLoadingMixin, {
  notify: Ember.inject.service(),
  store: Ember.inject.service(),
  i18n: Ember.inject.service(),

  /** @abstract */
  modalId: null,

  /** @abstract */
  label: null,

  open: false,

  error: null,

  /**
   * To inject
   * @type File
   */
  file: null,

  /*** Form values ***/
  shareName: null,
  shareType: DEFAULT_SHARE_TYPE,
  sharePublicAccess: DEFAULT_SHARE_PUBLIC_ACCESS,

  /**
   * Indicates that ``submit`` action is pending.
   */
  isSubmitting: false,

  init() {
    this._super();
    this.resetProperties();
  },

  isBusy: Ember.computed('isSubmitting', function() {
    return this.get('isSubmitting');
  }),

  isReadyToSubmit: Ember.computed('shareName', function() {
    return this.get('shareName');
  }),

  resetProperties() {
    this.setProperties({
      shareName: null,
      shareType: DEFAULT_SHARE_TYPE,
      sharePublicAccess: DEFAULT_SHARE_PUBLIC_ACCESS,
      error: null,
    });
  },

  actions: {
    open() {
      this.setProperties({
        shareName: this.get('file.name')
      });
    },

    opened() {
    },

    closed() {
      this.resetProperties();
    },

    submit() {
      const file = this.get('file');
      this.set('isSubmitting', true);

      const createPromise = this.get('oneproviderServer').createFileShare(
        file.get('id'),
        this.get('shareName'),
        this.get('shareType'),
        this.get('sharePublicAccess')
      );

      createPromise
        .then(() => this.submitSucceed())
        .catch((error) => this.submitFailed(error))
        .finally(() => this.submitCompleted());
    },

  },

  submitSucceed() {
    this.set('open', false);
    const msg = this.get('i18n').t('components.modals.fileShare.submitSuccess');
    this.get('notify').info(msg);
  },

  submitFailed(error) {
    // TODO: display it in a share modal as an alert panel
    const msg = this.get('i18n').t('components.modals.fileShare.submitFailed');
    this.get('notify').error(msg + (error && error.message && ': ' + error.message));
    console.error(msg);
  },

  submitCompleted() {
    this.setProperties({
      isSubmitting: false
    });
  }

});
