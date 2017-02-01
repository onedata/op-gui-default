import Ember from 'ember';
import DS from 'ember-data';

/**
 * Transforms ``Ember.Array`` (frontend) <-> JSON Array (backend).
 *
 * It adds support for using arrays as a model property type.
 *
 * @module transforms/array
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Transform.extend({
  deserialize: function(value) {
    if (Ember.isArray(value)) {
      return Ember.A(value);
    } else {
      return Ember.A();
    }
  },
  serialize: function(value) {
    if (Ember.isArray(value)) {
      return Ember.A(value);
    } else {
      return Ember.A();
    }
  }
});
