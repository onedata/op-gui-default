import Ember from 'ember';

/**
 * A generic modal that is used to fetch a token (see type property).
 * @module
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  i18n: Ember.inject.service(),
  oneproviderServer: Ember.inject.service(),
  notify: Ember.inject.service(),

  /** Options passed into  */
  options: null,

  inviteToken: null,

  isOpened: false,

  init() {
    this._super(...arguments);
    this.setProperties({
      isOpened: false,
    });
  },

  /** Id of HTML element with -modal suffix */
  modalId: function() {
    return `token-${this.get('type')}`;
  }.property('type'),

  modalTitle: function() {
    return this.get('i18n').t(`components.tokenModals.${this.get('type')}.title`);
  }.property('type'),

  modalLabel: function() {
    return this.get('i18n').t(`components.tokenModals.${this.get('type')}.label`);
  }.property('type'),

  formElementId: function() {
    return `invite-form-${this.get('modalId')}`;
  }.property('modalId'),

  clipboardTarget: function() {
    return `#${this.get('formElementId')} input`;
  }.property('formElementId'),

  selectTokenText() {
    let input = $('#invite-token-field')[0];
    $(input).focus();
    input.setSelectionRange(0, input.value.length);
  },

  actions: {
    getToken() {
      let type = this.get('type');
      let tokenFun = this.get('oneproviderServer')[`getToken${type.capitalize()}`];
      // TODO: handle in GUI?
      if (!tokenFun) {
        throw `GetToken function not found in oneProviderServer for type: ${type}`;
      }
      tokenFun.apply(this.get('oneproviderServer'), this.get('funArgs')).then(
        (data) => {
          this.set('inviteToken', data.token);
        },
        (error) => {
          this.set('errorMessage', error.message || this.get('i18n').t('common.unknownError'));
          console.error(`Token ${type} fetch failed: ` + JSON.stringify(error));
        }
      );
    },
    selectAll() {
      this.selectTokenText();
    },
    closeModal() {
      this.set('inviteToken', null);
      this.set('errorMessage', null);
    },
    copySuccess() {
      this.selectTokenText();
      this.get('notify').info(this.get('i18n').t('common.notify.clipboardSuccess'));
    },
    copyError() {
      this.selectTokenText();
      this.get('notify').warn(this.get('i18n').t('common.notify.clipboardFailure'));
    },
  }
});
