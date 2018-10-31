import Ember from 'ember';

/**
 * A modal for publishing a share by creating a ``handle`` record.
 *
 * @module modals/publish-share
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  notify: Ember.inject.service(),
  store: Ember.inject.service(),
  i18n: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  session: Ember.inject.service(),

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
   * A flag to indicate that fetched list of ``handleServices`` is empty
   * or unavailable.
   * @type {Boolean}
   */
  noHandleServicesAvailable: false,

  /**
   * MetadataString that will be added to handle created with this modal.
   * @type {String}
   */
  metadataString: '',

  /**
   * True if medatadaString is not valid
   * @type {boolean}
   */
  isMetadataIncorrect: false,

  init() {
    this._super();
    this.resetProperties();
  },

  fetchHandleServices: Ember.on('init', function() {
    let fetchPromise = this.get('session.user').get('handleServices');
    fetchPromise.then((hsList) => {
      if (hsList) {
        this.set('availableHandleServices', hsList);
      } else {
        this.set('noHandleServicesAvailable', true);
      }
    });

    // TODO: indicate error when handle services cannot be fetched
    fetchPromise.catch((error) => {
      console.error(`Cannot fetch handle services list due to an error: ${error.message}`);
      this.set('noHandleServicesAvailable', true);
    });
  }),

  metadataEditorDisabled: Ember.computed('handleService', function() {
    return !this.get('handleService');
  }),

  handleServicesNotEmpty: Ember.computed('availableHandleServices', function() {
    return this.get('availableHandleServices') &&
      this.get('availableHandleServices.length') > 0 &&
      !this.get('noHandleServicesAvailable');
  }),

  isBusy: Ember.computed.alias('isSubmitting'),

  isReadyToSubmit: Ember.computed(
    'handleService',
    'isMetadataIncorrect',
    function isReadyToSubmit() {
      return !!this.get('handleService') && !this.get('isMetadataIncorrect');
    }
  ),

  resetProperties() {
    this.setProperties({
      availableHandleServices: null,
      noHandleServicesAvailable: false,
      metadataString: '',
      isMetadataIncorrect: false,
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

    onMetadataChange(metadataString, isError) {
      this.setProperties({
        metadataString,
        isMetadataIncorrect: isError,
      });
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
        .then((handle) => this.submitSucceed(this.get('share'), handle))
        .catch((error) => this.submitFailed(this.get('share'), handle, error))
        .finally(() => this.submitCompleted());
    },

  },

  submitSucceed(share, handle) {
    console.debug(`Share handle created with publicHandle: ${handle.get('publicHandle')}`);
    this.get('notify').info(
      this.get('i18n').t('components.modals.publishShare.publishSuccess', {
        shareName: share.get('name')
      })
    );
  },

  submitFailed(share, handle, error) {
    this.get('notify').error(
      this.get('i18n').t('components.modals.publishShare.publishFailure', {
        errorMessage: error.message || 'unknown error',
        shareName: share.get('name')
      })
    );
    if (handle) {
      handle.rollbackAttributes();
    }
  },

  submitCompleted() {
    this.set('open', false);
  }

});
