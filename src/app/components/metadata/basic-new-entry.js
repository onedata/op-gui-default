import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',
  classNames: ['basic-new-entry'],
  classNameBindings: ['invalidClass'],

  /**
   * Truthy when we invoked action to submit this new entry to all basic
   * metadata entries (``submit``) and we wait for resolve.
   * @type {Boolean}
   */
  isSubmitting: false,

  /**
   * Push here array of existing keys, so this component can check is the newKey
   * valid (does not repeat).
   * @type {Array<String>}
   */
  existingKeys: null,

  init() {
    this._super(...arguments);
    if (!this.get('existingKeys')) {
      this.set('existingKeys', Ember.A());
    }
  },

  invalidClass: Ember.computed('isValid', 'newKey', function() {
    return !this.get('isValid') && this.get('newKey') ? 'invalid' : '';
  }),

  /**
   * The ``basic-new-entry`` editor is not empty, so we can add new empty
   * entry row.
   * @type {Boolean}
   */
  createNewEntryAvailable: Ember.computed('newKey', 'newValue', 'isValid', function() {
    const props = this.getProperties('newKey', 'newValue', 'isValid');
    return props.newKey && props.newValue && props.isValid;
  }),

  /**
   * If true, the basic new entry can be submitted up.
   * Otherwise, it is presented as invalid.
   * @type {Boolean}
   */
  isValid: Ember.computed('existingKeys.[]', 'newKey', 'newValue', function() {
    return this.get('newKey') && this.get('newValue') &&
      !this.get('existingKeys').includes(this.get('newKey'));
  }),

  /**
   * Handle user enter press on one of input fields, to focus him on proper
   * input field.
   */
  changeFocus() {
    if (this.get('newKey')) {
      this.$().find('.basic-new-entry-value')[0].focus();
    } else {
      this.$().find('.basic-new-entry-key')[0].focus();
    }
  },

  /**
   * Inform parent that we want to create new row to edit new entry. Currently
   * edited row should be attached to other basic metadata entries.
   */
  submit() {
    this.set('isSubmitting');
    const p = new Ember.RSVP.Promise((resolve) => {
      this.sendAction('createNewEntry', this.get('newKey'), this.get('newValue'), resolve);
    });
    p.then(() => {
      this.setProperties({
        newKey: null,
        newValue: null,
      });
    });
    p.finally(() => {
      this.set('isSubmitting', false);
    });
  },

  /**
   * Notify parent about changes in entry, so it can attach the new entry to
   * whole basic metadata.
   */
  valuesChanged: Ember.observer('newKey', 'newValue', 'isValid', function() {
    const props = this.getProperties('newKey', 'newValue', 'isValid');
    this.sendAction('entryChanged', props.newKey, props.newValue, props.isValid);
  }),

  actions: {
    /**
     * User wants to submit the new entry - if it is ready, invoke some actions
     * on the parent. Otherwise, do something to inform user that it is not
     * ready yet.
     */
    trySubmit() {
      if (this.get('createNewEntryAvailable')) {
        this.submit();
      } else {
        this.changeFocus();
      }
    },
  }
});
