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
 * Model with group permissions list for space.
 * @module models/group-group-list
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  space: belongsTo('space', { async: true }),
  
  systemGroups: attr('array'),
  
  systemGroupRecords: computed('space.id', 'systemGroups', function () {
    const systemGroups = this.get('systemGroups');
    const spaceId = this.belongsTo('space').id();
    if (spaceId != null) {
      return PromiseArray.create({
        promise: Promise.all(systemGroups.map(systemGroupId =>
          this.store.queryRecord('system-group', {
            id: systemGroupId,
            context: {
              od_space: spaceId,
            }
          })
        ))
      }); 
    }
  }),
});
