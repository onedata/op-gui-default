import Ember from 'ember';

const {
  Mixin,
  computed,
  String: {
    htmlSafe
  },
  inject: {
    service
  }
} = Ember;

const I18N_PREFIX = 'spacesError.';
const BR = '<br>';

/**
 * Generate error message based on model error for routes that use spaces collection.
 * 
 * @module mixins/controllers/spaces-error
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Mixin.create({
  i18n: service(),

  errorMessage: computed('model.type', function() {
    let i18n = this.get('i18n');
    let t = (msg) => i18n.t(I18N_PREFIX + msg);
    let message = '';
    switch (this.get('model.type')) {
      case 'empty':
        message = t('noSpaceSupported');
        break;
      default:
        message = t('fetchFailure');
        break;
    }
    message += BR + t('errorCauses') + BR + t('advice');
    return htmlSafe(message);
  })
});