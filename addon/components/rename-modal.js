import Ember from 'ember';
import layout from '../templates/components/rename-modal';

const {
  computed,
  observer
} = Ember;

const CHOICE_CLOSED = 0;
const CHOICE_OPENED = 1;
const CHOICE_CONFIRMED = 2;
const CHOICE_CANCELLED = 3;

const MODAL_TYPE = 'rename';

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
export default Ember.Component.extend({
  layout,

  model: null,
  renameName: null,

  /**
   * @public
   * @type {Boolean}
   */
  open: false,

  /**
   * @public
   * @type {String}
   */
  title: null,

  /**
   * @public
   * @type {String}
   */
  label: null,

  /**
   * A function that will be invoked when confirmed the dialog.
   * 
   * The function should return a Promise. When the promise is
   * in progress, the dialog submit button will be in progress.
   *
   * When the Promise finishes (resolves or rejects), the modal
   * will be closed.
   * @default null
   * @type {Function|null}
   * @public
   */
  onConfirm: null,

  /**
   * A function that will be invoked when cancelled the dialog
   * using "cancel" button or closing the modal in other way.
   * 
   * The function should return a Promise. When the promise is
   * in progress, the dialog "cancel" button will be in progress.
   * 
   * When the Promise finishes (resolves or rejects), the modal
   * will be closed.
   * @default null
   * @type {Function|null}
   * @public
   */
  onCancel: null,

  /**
   * Additional classes for button.
   * @default "btn-primary"
   * @type {String}
   * @public
   */
  btnClass: 'btn-primary',

  /**
   * State of choice made in this dialog.
   * See ``CHOICE_`` constants in this file.
   * @private
   */
  choice: CHOICE_CLOSED,

  /**
   * @private
   * @type {Boolean}
   */
  confirmInProgress: false,

  /**
   * @private
   * @type {Boolean}
   */
  cancelInProgress: false,

  init() {
    this._super(...arguments);
  },

  _resetProperties() {
    this.setProperties({
      title: null,
      label: null,
      choice: CHOICE_CLOSED
    });
  },

  canSubmit: Ember.computed('renameName', 'isLoading', 'open', function () {
    let {renameName, isLoading, open} = this.getProperties('renameName', 'isLoading', 'open');
    return renameName && !isLoading && open;
  }),

  isLoading: computed('confirmInProgress', 'cancelInProgress', function() {
    let {confirmInProgress, cancelInProgress} =
      this.getProperties('confirmInProgress', 'cancelInProgress');
      return confirmInProgress || cancelInProgress;
  }),

  handleChoice(confirmed = false) {
    let progressProperty = (confirmed ? 'confirm' : 'cancel') + 'InProgress';
    this.set(progressProperty, true);
    let fun = this.get(confirmed ? 'onConfirm' : 'onCancel');
    let finalFun = () => {
      this.set(progressProperty, false);
      this.send('close');
    };
    if (fun) {
      fun(confirmed ? this.get('renameName') : undefined).finally(finalFun);
    } else {
      finalFun();
    }
  },

  choiceChanged: observer('choice', function () {
    let choice = this.get('choice');
    if (choice === CHOICE_CONFIRMED || choice === CHOICE_CANCELLED) {
      this.handleChoice(choice === CHOICE_CONFIRMED);
    }
  }),

  actions: {
    confirm() {
      this.set('choice', CHOICE_CONFIRMED);
    },

    cancel() {
      this.set('choice', CHOICE_CANCELLED);
    },

    close() {
      this.sendAction('closeModal', MODAL_TYPE);
    },

    open() {
      this.setProperties({
        renameName: this.get('model.name'),
        choice: CHOICE_OPENED
      });
    },

    closed() {
      let choice = this.get('choice');
      if (choice === CHOICE_OPENED) {
        // choice not yet set - we closed the window, so we cancelled operation
        this.set('choice', CHOICE_CANCELLED);
      }
      this._resetProperties();
    },

    opened() {
      $(`#${this.get('modalId')}-modal`).find('input').focus()[0].select();
    },

  },
});
