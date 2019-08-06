/**
 * Database view with which a set of files can be transferred
 *
 * @module models/db-view
 * @author Jakub Liput, Michał Borzęcki
 * @copyright (C) 2018-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';
import Ember from 'ember';
import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';

const {
  Model,
  attr,
  belongsTo,
} = DS;

const {
  computed,
  RSVP: { Promise },
} = Ember;


export default Model.extend({  
  name: attr('string'),
  space: belongsTo('space', { async: true, inverse: null }),
  providers: attr('array'),
  spatial: attr('boolean'),
  viewOptions: attr('object'),
  mapFunction: attr('string'),
  reduceFunction: attr('string'),
  
  providersRecords: computed(
    'providers',    
    function() {
      const {
        store,
        providers,
      } = this.getProperties('store', 'providers');
      const promises = providers.map(providerId => {
        return store.queryRecord('system-provider', {
          id: providerId,
          context: {
            od_space: this.belongsTo('space').id(),
          }
        });
      });
      return PromiseArray.create({ promise: Promise.all(promises) });
    }
  ),
});
