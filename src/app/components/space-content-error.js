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

  i18nPrefix: 'components.spaceContentError.',

  onezoneUrl: reads('session.sessionDetails.onezoneURL'),
  providerApiOrigin: reads('error.provider.apiOrigin'),
  space: reads('error.space'),
  providers: reads('error.providers'),

  spaceSupportUrl: computed('onezoneURL', 'space.id', function spaceSupportUrl() {
    return `${this.get('onezoneUrl')}/#/onedata/spaces/${this.get('space.id')}/support`;
  }),

  actions: {
    refreshRoute() {
      this.get('error.transition').retry();
    },
  },
});
