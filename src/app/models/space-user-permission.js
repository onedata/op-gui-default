import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-gui-worker/mixin-factories/models/permission';
import FLAG_NAMES from 'op-gui-worker/constants/permission-space-flags';

const {
  computed,
  RSVP: {
    Promise
  },
  inject
} = Ember;

const {
  belongsTo
} = DS;

const ObjectPromiseProxy =
  Ember.ObjectProxy.extend(Ember.PromiseProxyMixin);

/**
 * A set of single Space permissions for a single User
 * @module models/space-user-permission
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(permissionModelFactory(FLAG_NAMES), {
  store: inject.service(),

  systemUser: computed('space.id', function () {
    let store = this.get('store');
    
    let mainPromise = new Promise((resolve, reject) => {
      let getSpace = this.get('space');
      getSpace.then(space => {
        let spaceId = space.get('id');
        let getSystemUser = store.queryRecord('systemUser', {
          context: {
            od_space: spaceId
          }
        });
        getSystemUser.then(resolve, reject);
      });
      getSpace.catch(reject);
    });
    
    return ObjectPromiseProxy.create({
      promise: mainPromise
    });
  }),

  space: belongsTo('space', {async: true, inverse: null}),

  /** Common alias for owner - in this case a user (system-user) */
  owner: computed.alias('systemUser'),
});
