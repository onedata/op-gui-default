import Ember from 'ember';

/**
 * Custom single radio button for use in ``one-radio-group``.
 * Only one radio button can be checked in group.
 * For multi-choice see ``one-checkbox-button``.
 *
 * A ``groupValue`` is binding to property which will contain a ``value``
 * of currently selected radio.
 *
 * @module components/one-radio-button
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['one-option-button', 'col-xs-4'],

  checked: function() {
    return this.get('groupValue') === this.get('value');
  }.property('groupValue', 'value'),

  groupValue: null,
  label: null,
  value: null,

  icon: function() {
    return this.get('checked') ? 'checkbox-option' : 'checkbox-empty';
  }.property('checked'),

  toggle() {
    this.set('groupValue', this.get('value'));
  },

  click() {
    this.toggle();
  }
});
