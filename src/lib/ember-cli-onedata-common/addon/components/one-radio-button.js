import Ember from 'ember';
import layout from 'ember-cli-onedata-common/templates/components/one-radio-button';

/**
 * Custom single radio button for use in ``one-radio-group``.
 * Only one radio button can be checked in group.
 * For multi-choice see ``one-checkbox-button``.
 *
 * A ``groupValue`` is binding to property which will contain a ``value``
 * of currently selected radio.
 *
 * @todo refactor to follow data down / actions up (now binds up to groupValue)
 * @module components/one-radio-button
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  layout,

  classNames: ['one-option-button', 'col-xs-4'],
  classNameBindings: ['checked:checked:not-checked'],

  checked: Ember.computed('groupValue', 'value', function() {
    return this.get('groupValue') === this.get('value');
  }),

  groupValue: null,
  label: null,
  value: null,

  icon: Ember.computed('checked', function() {
    return this.get('checked') ? 'checkbox-option' : 'checkbox-empty';
  }),

  toggle() {
    this.set('groupValue', this.get('value'));
  },

  click() {
    this.toggle();
  }
});
