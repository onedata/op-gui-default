import Ember from 'ember';

/**
 * FIXME: description
 *
 * @module modals/publish-share
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
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
   * @type {Share|SharePublic}
   */
  share: null,

  /**
   * Indicates that ``submit`` action is pending.
   */
  isSubmitting: false,

  /**
   * Stores list of ``handle-services`` that can be selected.
   * It is filled
   * @type {[type]}
   */
  availableHandleServices: null,

  /**
   * MetadataString that will be added to handle created with this modal.
   * @type {String}
   */
  metadataString: null,

  init() {
    this._super();
    this.resetProperties();
  },

  fetchHandleServices: Ember.on('init', function() {
    const fetchPromise = this.get('store').findAll('handle-service');
    fetchPromise.then((hsList) => {
      if (hsList) {
        this.set('availableHandleServices', hsList);
      } else {
        // FIXME: handle empty handle services
      }
    });
  }),

  handleServicesNotEmpty: Ember.computed('availableHandleServices', function() {
    return this.get('availableHandleServices');
  }),

  isBusy: Ember.computed.alias('isSubmitting'),

  isReadyToSubmit: Ember.computed('metadataString', function() {
    return !!this.get('handleService') && !!this.get('metadataString');
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

    handleServiceChanged(handleService) {
      this.set('handleService', handleService);
    },

    submit() {
      this.set('isSubmitting', true);

      const handle = this.get('store').createRecord('handle', {
        share: this.get('share'),
        handleService: this.get('handleService'),
        metadataString: this.get('metadataString'),
      });

      this.set('share.handle', handle);

      const savePromise = handle.save();

      savePromise
        .then((handle) => this.submitSucceed(handle))
        .catch((error) => this.submitFailed(error))
        .finally(() => this.submitCompleted());
    },

  },

  submitSucceed(handle) {
    console.debug(`Share handle created with publicHandle: ${handle.get('publicHandle')}`);
  },

  submitFailed(error) {
    // FIXME: handle errors
    console.error(`Publish share error: ${error}`);
  },

  submitCompleted() {
    this.set('open', false);
  }

});
