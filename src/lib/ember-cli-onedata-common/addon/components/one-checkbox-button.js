import Ember from 'ember';

/**
 * Custom single checkbox for use in ``one-checkbox-group``.
 * All checkboxes in group share single Ember object, which has boolean properties
 * name as ``this.value``.
 *
 * For example we can have an object somewhere:
 * ```
 * let allValues = Ember.Object.create({
 *  one: false,
 *  two: false
 * });
 * ```
 *
 * Then we set ``allValues`` as a ``groupValues`` property of this component (HBS)
 * and name of the property to change:
 * ```
 * {{#one-checkbox-group as |allValues|}}
 *   {{one-checkbox-button groupValues=allValues value="one" label="One"}}
 *   {{one-checkbox-button groupValues=allValues value="two" label="Two"}}
 * {{/one-checkbox-group}
 * ```
 *
 * Toggling pseudo-checkbox One and Two in GUI, will toggle ``allValues.one`` and ``allValues.two``.
 *
 * @module components/one-checkbox-button
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['one-option-button'],

  /**
   * Binding to object with properties, whose will be toggled
   * when specific checkboxes will be toggled (see module doc for details).
   * @type Ember.Object
   */
  groupValues: null,
  label: null,

  /**
   * Name of property of ``groupValues`` which will be toggled when checkbox is toggled.
   * @type string
   */
  value: null,

  init() {
    this._super();

    const prop = `groupValues.${this.get('value')}`;

    // groupValues can be not provided - use internal
    if (!this.get('groupValues')) {
      this.set('groupValues', Ember.Object.create());
    }

    this.checked = Ember.computed(prop, {
      get() {
        return this.get(prop);
      },
      set(key, value) {
        this.set(prop, value);
        return value;
      }
    });
  },

  icon: function() {
    return this.get('checked') ? 'checkbox-filled' : 'checkbox-empty';
  }.property('checked'),

  toggle() {
    this.set('checked', !this.get('checked'));
  },

  click() {
    this.toggle();
  }
});
