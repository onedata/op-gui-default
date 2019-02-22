import Ember from 'ember';
import PromiseLoadingMixin from 'ember-cli-onedata-common/mixins/promise-loading';

export default Ember.Component.extend(PromiseLoadingMixin, {
  notify: Ember.inject.service(),

  modalId: 'provider-redirect',

  title: computed(function title() {
    return this.get('i18n', 'components.modals.providerRedirect.title')
  }),

  open: false,

  isLoading: false,

  model: null,

  actions: {
    open() {
    },

    opened() {
    },

    submit() {
      this.promiseLoading(
        new Ember.RSVP.Promise((resolve, reject) => {
          this.sendAction('answered', true, this.get('model'), resolve, reject);
        })
      ).finally(() => {
        this.setProperties({
          model: null,
          open: false
        });
      });
    },
  }


});
