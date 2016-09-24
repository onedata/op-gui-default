import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',
  classNameBindings: ['isRecent:animated', 'isRecent:flash', ''],

  /**
   * To inject.
   * Truthy if this entry should flash itself, because it was recently added.
   * @type {Boolean}
   */
  isRecent: false,

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
    if (this.get('isRecent')) {
      this.sendAction('clearRecentKey');
    }
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
