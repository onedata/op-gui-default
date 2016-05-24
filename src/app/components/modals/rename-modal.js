import Ember from 'ember';
import PromiseLoadingMixin from '../../mixins/promise-loading';

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
  renameName: null,

  actions: {
    open() {
      this.set('renameName', this.get('model.name'));
    },

    opened() {
      $(`#${this.get('modalId')}-modal`).find('input').focus()[0].select();
    },

    submit() {
      this.set('isLoading', true);
      let model = this.get('model');
      let oldName = model.get('name');
      let newName = this.get('renameName');
      model.set('name', this.get('renameName'));

      this.promiseLoading(
        model.save()
      ).then(
        () => {
          this.get('notify').info(this.get('i18n').t(
            'components.modals.renameModal.renameSuccess', {
              oldName: oldName,
              newName: newName
            }
          ));
        },
        (error) => {
          this.get('notify').error(this.get('i18n').t(
            'components.modals.renameModal.renameFailed', {
              oldName: oldName,
              newName: newName
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
