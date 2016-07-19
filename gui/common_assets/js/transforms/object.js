import Ember from 'ember';
import DS from 'ember-data';

/**
 * Transforms ``Object`` (frontend) <-> JSON (backend).
 *
 * It adds support for using object as a model property type.
 *
 * @module transforms/object
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Transform.extend({
  deserialize: function(value) {
    if (!Ember.$.isPlainObject(value)) {
      return {};
    } else {
      return value;
    }
  },
  serialize: function(value) {
    if (!Ember.$.isPlainObject(value)) {
      return {};
    } else {
      return value;
    }
  }
});
