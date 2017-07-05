/**
 * A function that adds a ``conflictLabel`` property for each conflicting record
 * in some array.
 * 
 * For example, we can have two spaces with the same name. This function will add
 * a property that helps to dinstinguish these two.
 *
 * @module utils/add-conflict-labels
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import conflictIds from 'ember-cli-onedata-common/utils/conflict-ids';

const {
  get,
  set,
} = Ember;

/**
 * Assigns a ``conflictLabel`` property for each record in array.
 * See utils/conflict-ids for details about conflict ids algorithm.
 * @param {Array} records 
 * @param {string} conflictProperty 
 * @param {string} diffProperty 
 * @param {string} [defaultId]
 */
export default function addConflictLabels(records, conflictProperty = 'name', diffProperty = 'id', defaultId = undefined) {
  let conflictPropertyMap = new Map();
  records.forEach(record => {
    let conflictValue = get(record, conflictProperty);
    if (conflictPropertyMap.has(conflictValue)) {
      conflictPropertyMap.get(conflictValue).push(record);
    } else {
      conflictPropertyMap.set(conflictValue, [record]);
    }
  });
  
  conflictPropertyMap.forEach(conflictingRecords => {
    if (conflictingRecords.length > 1) {
      let conflictLabels = conflictIds(conflictingRecords.mapBy(diffProperty));
      for (let i = 0; i < conflictingRecords.length; i += 1) {
        let record = conflictingRecords[i];
        set(
          record,
          'conflictLabel',
          get(record, diffProperty) === defaultId ? null : conflictLabels[i]
        );
      }
    } else if (conflictingRecords.length === 1) {
      set(conflictingRecords[0], 'conflictLabel', null);
    }
  });
}
