import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';

const {
  computed,
  computed: { reads },
  get,
  observer,
  inject: { service },
} = Ember;

export default Ember.Component.extend(PromiseLoadingMixin, {
  classNames: ['provider-redirect'],
  
  session: service(),

  modalId: 'provider-redirect',

  currentProviderName: reads('session.sessionDetails.providerName'),
  
  title: computed(function title() {
    return this.get('i18n').t('components.modals.providerRedirect.title');
  }),

  /**
   * @virtual
   * @type {boolean}
   */
  open: false,

  /**
   * @virtual
   * @type {RSVP.Deferred}
   */
  deferredProviderChoice: undefined,

  /**
   * @virtual
   * @type {Function}
   */
  close: () => {},
  
  /**
   * @virtual
   * @type {Array<models/SystemProvider}
   */
  providers: undefined,

  chosenProvider: undefined,
  
  isLoading: computed(
    'providers.@each.isLoading',
    function isLoading() {
      return this.get('providers').some(p => get(p, 'isLoading'));
    }
  ),

  providerOptions: computed(
    'providers.@each.{online,name}',
    function providerOptions() {
      const i18n = this.get('i18n');
      const options = this.get('providers').map(provider => ({
        id: get(provider, 'id'),
        text: get(provider, 'name'),
        disabled: !get(provider, 'online'),
        description: !get(provider, 'online') ? i18n.t('components.modals.providerRedirect.providerItem.offline') : null,
        provider,
      }));
      options.sort((a, b) => {
        const aOnline = !get(a, 'disabled');
        const bOnline = !get(b, 'disabled');
        if (aOnline && !bOnline) {
          return -1;
        } else if (aOnline && bOnline || !aOnline && !bOnline) {
          const aName = get(a, 'text');
          const bName = get(b, 'text');
          return aName.localeCompare(bName);
        } else {
          return 1;
        }
      });
      return options;
    }
  ),
  
  providerSelectionChanged: observer(
    'chosenProvider',
    function providerSelectionChanged() {
      const chosenProvider = this.get('chosenProvider');
      if (chosenProvider) {
        this.providerChosen(chosenProvider);
      }
    }
  ),
  
  providerChosen(providerItem) {
    this.get('deferredProviderChoice').resolve(get(providerItem, 'provider'));
    this.get('close')();
  },
  
  actions: {
    cancel() {
      this.get('close')();
      this.get('deferredProviderChoice').resolve(null);
    },
  }


});
