import Ember from 'ember';
import PromiseLoadingMixin from 'op-worker-gui/mixins/promise-loading';

/**
 * Modal that allows to create a Share from selected directory.
 *
 * @module modals/create-share
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(PromiseLoadingMixin, {
  notify: Ember.inject.service(),
  store: Ember.inject.service(),
  i18n: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),

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

  /**
   * To inject
   * @type DataSpace
   */
  dataSpace: null,

  /*** Form values ***/
  shareName: null,

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
      error: null,
      file: null,
      isSubmitting: false,
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
        this.get('shareName')
      );

      createPromise
        .then((createData) => this.submitSucceed(createData))
        .catch((error) => this.submitFailed(error))
        .finally(() => this.submitCompleted());
    },

  },


  /**
   * Handle successful resolve of ``createFileShare`` - a shareId is resolved
   * so use it to fetch created Share record.
   *
   * @param  {Object} shareCreatedData object resolved from
   *    ``oneproviderServer.createFileShare`` RPC
   */
  submitSucceed(shareCreatedData) {
    const msg = this.get('i18n').t('components.modals.createShare.submitSuccess');
    this.get('notify').info(msg);

    // use a get share promise to open a share info modal
    this.sendAction(
      'openShareInfoModal',
      this.get('store').findRecord('share', shareCreatedData.shareId)
    );

    this.set('open', false);
  },

  submitFailed(error) {
    // TODO: display it in a share modal as an alert panel
    const msg = this.get('i18n').t('components.modals.createShare.submitFailed');
    this.get('notify').error(msg + (error && error.message && ': ' + error.message));
  },

  submitCompleted() {
    this.set('open', false);
  }

});
