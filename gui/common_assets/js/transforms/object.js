import Ember from 'ember';
import DS from 'ember-data';

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
