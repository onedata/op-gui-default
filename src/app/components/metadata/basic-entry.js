import Ember from 'ember';

export default Ember.Component.extend({
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
   * @type {String}
   */
  value: null,

  init() {
    this._super(...arguments);
  },

  /**
   * Value of entry changed - notify ``basic-editor``.
   */
  valueChanged: Ember.observer('value', function() {
    this.sendAction('entryChanged', this.get('key'), this.get('value'));
  }),

  actions: {
    remove() {
      this.set('disabled', true);
      const p = new Ember.RSVP.Promise((resolve) => {
        this.sendAction('removeEntry', this.get('key'), resolve);
      });
      p.finally(() => {
        if (this) {
          this.set('disabled', false);
        }
      });
    }
  }
});
