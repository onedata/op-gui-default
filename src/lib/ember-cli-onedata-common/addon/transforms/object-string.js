import Ember from 'ember';
import DS from 'ember-data';

/**
 * Transforms stringified object [string] (frontend) <-> JSON [object] (backend).
 *
 * It adds support for using object as a model property type, but in contrast to
 * "object" transform, it supports attributes isDirty check.
 *
 * @module transforms/object-string
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Transform.extend({
  deserialize: function(value) {
    if (Ember.$.isPlainObject(value)) {
      return JSON.stringify(value);
    } else {
      return '{}';
    }
  },
  serialize: function(value) {
    if (typeof value === 'string') {
      return JSON.parse(value);
    } else {
      return {};
    }
  }
});
