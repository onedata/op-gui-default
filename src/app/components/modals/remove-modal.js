import Ember from 'ember';
import PromiseLoadingMixin from 'op-worker-gui/mixins/promise-loading';

export default Ember.Component.extend(PromiseLoadingMixin, {
  notify: Ember.inject.service(),

  /** @abstract */
  modalId: null,

  /** @abstract */
  title: null,

  /** @abstract */
  label: null,

  open: false,

  isLoading: false,

  model: null,

  actions: {
    open() {
    },

    opened() {
    },

    submit() {
      this.set('isLoading', true);
      let model = this.get('model');
      const modelName = model.get('name');

      this.promiseLoading(
        model.destroyRecord()
      ).then(
        () => {
          this.get('notify').info(this.get('i18n').t(
            'components.modals.removeModal.removeSuccess', {
              name: modelName
            }
          ));
        },
        (error) => {
          this.get('notify').error(this.get('i18n').t(
            'components.modals.removeModal.removeFailed', {
              name: modelName
            }
          ) + ': ' + error.message);
          model.rollbackAttributes();
        }
      ).finally(() => {
        this.setProperties({
          model: null,
          open: false
        });
      });
    },
  }


});
