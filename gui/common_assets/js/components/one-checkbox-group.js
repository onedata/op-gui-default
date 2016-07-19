import Ember from 'ember';

/**
 * A container for ``one-checkbox-button``s.
 * It yields the object that will contain ``groupValues``
 * which should be injected to ``one-checkbox-button``s.
 *
 * @module components/one-checkbox-group
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['one-option-group'],

  init() {
    this._super();

    // this property will contain a dictionary of checkbox options
    if (this.get('groupValues') == null) {
      this.set('groupValues', Ember.Object.create());
    }
  }
});
