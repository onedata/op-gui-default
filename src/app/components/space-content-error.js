import Ember from 'ember';

const {
  Component,
  inject: { service },
  computed,
  computed: { reads },
} = Ember;

export default Component.extend({
  session: service(),
  router: service(),

  /**
   * @virtual
   * Properties: type, space, provider (opt)
   * @type {object}
   */
  error: undefined,

  onezoneUrl: reads('session.sessionDetails.onezoneURL'),

  space: reads('error.space'),

  spaceSupportUrl: computed('onezoneURL', 'space.id', function spaceSupportUrl() {
    return `${this.get('onezoneUrl')}/#/onedata/spaces/${this.get('space.id')}/support`;
  }),

  // FIXME: debug
  init() {
    this._super(...arguments);
  },

  actions: {
    refreshRoute() {
      this.get('error.transition').retry();
    },
  },
});
