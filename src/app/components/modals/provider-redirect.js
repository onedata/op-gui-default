/**
 * When space or other resource is not supported on current Oneprovider
 * this modal asks for choosing other provider to redirect.
 * It resolves `deferredProviderChoice` with chosen `provider` model.
 * 
 * @module components/modals/provider-redirect
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';
import sortDisabledAndText from 'op-worker-gui/utils/sort-disabled-and-text';

const {
  computed,
  computed: { reads, equal },
  get,
  observer,
  inject: { service },
} = Ember;

export default Ember.Component.extend(PromiseLoadingMixin, {
  classNames: ['provider-redirect'],

  session: service(),
  i18n: service(),

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

  isSingleProvider: equal('providers.length', 1),

  isLoading: computed(
    'providers.@each.isLoading',
    function isLoading() {
      return this.get('providers').isAny('isLoading');
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
        description: !get(provider, 'online') ? i18n.t(
          'components.modals.providerRedirect.providerItem.offline') : null,
        provider,
      }));
      options.sort(sortDisabledAndText);
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

  resolveProviderChoiceAndClose(provider) {
    this.get('deferredProviderChoice').resolve(provider);
    this.get('close')();
  },

  providerChosen(providerItem) {
    this.resolveProviderChoiceAndClose(get(providerItem, 'provider'));
  },

  actions: {
    goToSingleProvider() {
      this.resolveProviderChoiceAndClose(this.get('providers.firstObject'));
    },
    cancel() {
      this.resolveProviderChoiceAndClose(null);
    },
  }
});
