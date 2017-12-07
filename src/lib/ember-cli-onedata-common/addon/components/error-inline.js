import Ember from 'ember';
import layout from '../templates/components/error-inline';

const {
  Component,
  computed,
  inject: { service },
} = Ember;

export default Component.extend({
  layout,
  tagName: 'span',
  classNames: ['error-inline'],
  
  i18n: service(),
  
  /**
   * Hint shown on hover
   * @type {string}
   */
  hint: ci18n('components.errorInline.defaultHint'),
});

function ci18n(id) {
  return computed('i18n', function () {
    return this.get('i18n').t(id);
  });
}
