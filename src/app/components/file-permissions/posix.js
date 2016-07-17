import Ember from 'ember';

const DEFAULT_POSIX = 644;

// special POSIX codes used to recognize special situations in frontend
const POSIX_SPECIAL_DIFFERENT = 800;

export default Ember.Component.extend({
  permissions: null,

  error: null,

  /**
   * One of: different
   * @type String
   */
  status: Ember.computed('permissions', function() {
    if (this.get('permissions') === POSIX_SPECIAL_DIFFERENT) {
      return 'different';
    } else {
      return null;
    }
  }),

  init() {
    this._super();
    this.set('modal.posixComponent', this);
  },

  arePermissionsValid: Ember.computed('permissions', function() {
    const p = this.get('permissions');
    return p != null && p >= 0 && p <= 777;
  }),

  willDestroyElement() {
    this._super();
    this.set('modal.posixComponent', null);
  },

  isReadyToSubmit: Ember.computed('error', 'arePermissionsValid', function() {
    return !this.get('error') && this.get('arePermissionsValid');
  }),

  actions: {
    submit() {
      // TODO: submitting posix perms form should submit a parent modal
      this.sendAction("submit");
    },

    resetPermissions() {
      this.set('permissions', DEFAULT_POSIX);
    }
  }
});

export { DEFAULT_POSIX, POSIX_SPECIAL_DIFFERENT };
