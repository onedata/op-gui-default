import DS from 'ember-data';
import Ember from 'ember';
import ACL from '../utils/access-control-entity';

/**
 * Transforms array of AccessControlEntity.
 * It converts array of JSON (backend) <-> array of AccessControlEntity (application).
 * @module transforms/acl-array
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Transform.extend({
  deserialize: function(value) {
    if (Ember.isArray(value)) {
      return Ember.A(value.map(e => ACL.create(e)));
    } else {
      return Ember.A();
    }
  },
  serialize: function(value) {
    if (Ember.isArray(value)) {
      return Ember.A(value.map(e => e.toJSON()));
    } else {
      return Ember.A();
    }
  }
});
