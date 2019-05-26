import Ember from 'ember';

const {
  Component,
  computed,
  RSVP: { Promise },
} = Ember;

export default Component.extend({
  tagName: 'tr',

  /**
   * To inject.
   * If true, entry cannot be removed.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  /**
   * To inject
   * @type {String}
   */
  key: null,
  
  /**
   * To inject.
   * @type {any}
   */
  value: null,
  
  _stringValue: computed('value', function getStringValue() {
    const value = this.get('value');
    if (value && value instanceof Object) {
      try {
        return JSON.stringify(value);
      } catch (error) {
        return value;
      }
    } else {
      return value && value.toString();
    }
  }),
  
  /**
   * @virtual
   * @type {Function}
   */
  removeEntry: () => {},
  
  actions: {
    remove() {
      this.set('disabled', true);
      const p = new Promise((resolve) => {
        this.get('removeEntry')(this.get('key'), resolve);
      });
      p.finally(() => {
        if (this) {
          this.set('disabled', false);
        }
      });
    }
  }
});
