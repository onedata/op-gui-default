import Ember from 'ember';

const {
  Component,
  computed,
  String: { htmlSafe },
  inject: { service },
} = Ember;

const queryStringRegex = /(.*?)(\=)(.*)(&)|(.*?)(\=)(.*)/g;
const queryStringReplace = '<span class="query-string-key">$1$5</span><span class="query-string-equal">$2$6</span><span class="query-string-value">$3$7</span><span class="query-string-amp">$4</span>';

export default Component.extend({
  classNames: ['query-options'],
  
  i18n: service(),
  notify: service(),
  
  /**
   * @virtual
   * Object where keys are query parameter keys and values are values
   * @type {object}
   */
  options: undefined,
  
  queryString: computed('options', function queryString() {
    const options = this.get('options');
    if (options) {
      return $.param(options);
    }
  }),
  
  queryStringColorized: computed('queryString', function queryStringColorized() {
    const queryString = this.get('queryString');
    if (queryString) {
      return htmlSafe(queryString.replace(queryStringRegex, queryStringReplace));
    }
  }),
  
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
