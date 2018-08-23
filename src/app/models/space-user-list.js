import Ember from 'ember';
import DS from 'ember-data';
import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';

const {
  belongsTo,
  attr,
} = DS;

const {
  computed,
  RSVP: { Promise },
} = Ember;

/**
 * Model with group permissions list for group.
 * @module models/group-group-list
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  space: belongsTo('space', { async: true }),
  
  systemUsers: attr('array'),
  
  systemUserRecords: computed('space.id', 'systemUsers', function () {
    const systemUsers = this.get('systemUsers');
    const spaceId = this.belongsTo('space').id();
    if (spaceId != null) {
      return PromiseArray.create({
        promise: Promise.all(systemUsers.map(systemUserId =>
          this.store.queryRecord('system-user', {
            id: systemUserId,
            context: {
              od_space: spaceId,
            }
          })
        ))
      }); 
    }
  }),
});
