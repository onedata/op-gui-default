import Ember from 'ember';
import layout from '../templates/components/dialog-modal';

const {
  computed,
  observer
} = Ember;

const DEFAULT_TYPE = 'question';

const CHOICE_CLOSED = 0;
const CHOICE_OPENED = 1;
const CHOICE_CONFIRMED = 2;
const CHOICE_CANCELLED = 3;

/**
 * A generic dialog modal.
 * To use, set ``title`` and ``label`` properties and bind ``open`` property.
 * Set also ``onConfirm`` and ``onCancel`` (optional) properties with functions
 * to invoke on confirmation or dialog cancelation. These functions should
 * return Promises, so this dialog will wait for Promises to complete, displaying
 * buttons in "in progress" state. See ``onConfirm`` and ``onCancel`` properties
 * for more details.
 * 
 * When this modal is closed it sends ``closeModal("dialog")`` action up,
 * so container of this component should bind this action and change property
 * bound to ``open`` to false then.
 * 
 * For changing visual type of dialog modal, see ``type`` and ``btnClass`` properties.
 * 
 * @module components/dialog-modal
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  layout,

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
   * in progress, the dialog "yes" button will be in progress.
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
   * using "no" button or closing the modal in other way.
   * 
   * The function should return a Promise. When the promise is
   * in progress, the dialog "no" button will be in progress.
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
   * A type of status-panel used in dialog-modal.
   * See ``status-panel`` component ``type`` property doc
   * for more details of supported types.
   * @type {String}
   * @public
   */
  type: DEFAULT_TYPE,

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
      type: DEFAULT_TYPE,
      choice: CHOICE_CLOSED
    });
  },

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
      fun().finally(finalFun);
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
      this.sendAction('closeModal', 'dialog');
    },

    open() {
      this.set('choice', CHOICE_OPENED);
    },

    closed() {
      let choice = this.get('choice');
      if (choice === CHOICE_OPENED) {
        // choice not yet set - we closed the window, so we cancelled operation
        this.set('choice', CHOICE_CANCELLED);
      }
      this._resetProperties();
    },
  }


});
