import Ember from 'ember';

export default Ember.Component.extend({
  notify: Ember.inject.service(),
  i18n: Ember.inject.service(),

  classNames: ['input-with-button'],

  clipboardTarget: Ember.computed('elementId', function() {
    return `#${this.get('elementId')} input[type=text]`;
  }),

  selectTokenText() {
    let input = $(this.get('clipboardTarget'))[0];
    $(input).focus();
    input.setSelectionRange(0, input.value.length);
  },

  actions: {
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
