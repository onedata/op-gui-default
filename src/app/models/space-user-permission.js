import Ember from 'ember';
import DS from 'ember-data';
import permissionModelFactory from 'op-worker-gui/mixin-factories/models/permission';
import FLAG_NAMES from 'op-worker-gui/constants/permission-space-flags';

const {
  computed,
  RSVP: {
    Promise
  }
} = Ember;

const {
  attr,
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
  space: belongsTo('space', {async: true, inverse: null}),

  systemUserId: attr('string'),
  
  systemUser: computed('space.id', function () {
    let store = this.get('store');
    
    let mainPromise = new Promise((resolve, reject) => {
      let systemUserId = this.get('systemUserId');
      let getSpace = this.get('space');
      getSpace.then(space => {
        if (!space) {
          resolve(null);
        } else {
          let spaceId = space.get('id');
          let getSystemUser = store.query('system-user', { filter: {
            id: systemUserId,
            context: {
              od_space: spaceId
            }
          }});
          getSystemUser.then(resolve, reject);
        }
      });
      getSpace.catch(reject);
    });
    
    return ObjectPromiseProxy.create({
      promise: mainPromise
    });
  }),

  /** Common alias for owner - in this case a user (system-user) */
  owner: computed.alias('systemUser'),
});
