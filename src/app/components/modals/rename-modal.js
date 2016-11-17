import Ember from 'ember';
import PromiseLoadingMixin from 'op-worker-gui/mixins/promise-loading';

/**
 * A modal to change name of some model.
 * The model (injected property) should have ``name`` property
 * that will be changed with submit of this modal.
 * 
 * It sends actions:
 * - ``renameDone({success, oldName, model, error})`` - sent after rename submit
 *                                                      promise resolve/reject
 * @module components/modals/rename-modal
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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

  /**
   * To inject.
   * If true, show the notify.
   * Set to false if want to handle notification on your own.
   */
  useNotify: true,

  canSubmit: Ember.computed('renameName', 'isLoading', 'open', function () {
    let props = this.getProperties('renameName', 'isLoading', 'open');
    return props.renameName && !props.isLoading && props.open;
  }),

  actions: {
    open() {
      this.set('renameName', this.get('model.name'));
    },

    opened() {
      $(`#${this.get('modalId')}-modal`).find('input').focus()[0].select();
    },

    submit() {
      if (this.get('open')) {
        this.set('isLoading', true);
        let {model, renameName, useNotify, notify} = this.getProperties(
          'model', 'renameName', 'useNotify', 'notify'
        );
        let oldName = model.get('name');
        model.set('name', this.get('renameName'));

        this.promiseLoading(
          model.save()
        ).then(
          () => {
            if (useNotify) {
              notify.info(this.get('i18n').t(
                'components.modals.renameModal.renameSuccess', {
                  oldName: oldName,
                  newName: renameName
                }
              ));
            }
            this.sendAction('renameDone', {
              success: true,
              model: model,
              oldName: oldName
            });
          },
          (error) => {
            if (useNotify) {
              notify.error(this.get('i18n').t(
                'components.modals.renameModal.renameFailed', {
                  oldName: oldName,
                  newName: renameName
                }
              ) + ': ' + error.message);
            }
            model.rollbackAttributes();
            this.sendAction('renameDone', {
              success: false,
              oldName: oldName,
              model: model,
              error: error
            });
          }
          ).finally(() => {
            this.setProperties({
              model: null,
              open: false
            });
          });
      } else {
        console.warn(`rename-modal: submit invoked when open === false - ignoring`);
      }

    },
  }


});
