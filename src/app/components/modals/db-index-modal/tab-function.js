import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({
  classNames: ['tab-function'],
  
  /**
   * @virtual
   */
  functionString: undefined,
  
  /**
   * @virtual optional
   * Message that will be shown if the function is empty
   */
  emptyMessage: '',
  
  formattedSource: computed('functionString', function formattedSource() {
    const functionString = this.get('functionString');
    if (functionString) {
      const unescaped = functionString
        .replace(/\\\\/g, '\\')
        .replace(/\\"/g, '"')
        .replace(/\\'/g, '\'')
        .replace(/\\n/g, '\n')
        .replace(/\\t/g, '\t')
        .replace(/\\v/g, '\v')
        .replace(/\\0/g, '\f')
        .replace(/\\r/g, '\r');
      return window.js_beautify(unescaped);
    }
    
  }),
});
