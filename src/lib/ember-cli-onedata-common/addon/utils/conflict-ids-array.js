/**
 * An ArrayProxy that watches changes in source array and adds distinguishable
 * label to each object in that array using ``addConflictLabels`` function.
 *
 * @module utils/conflict-ids-array
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

import addConflictLabels from 'ember-cli-onedata-common/utils/add-conflict-labels';

const {
  ArrayProxy,
  observer,
  isArray,
} = Ember;

export default ArrayProxy.extend({
  /**
   * To inject.
   * @type {Array}
   */
  content: null,
  
  /**
   * Which property of record should be used to create conflict ID.
   * @type {string}
   * @default 'id'
   */
  diffProperty: undefined,
  
  /**
   * Which property should not be the same.
   * @type {string}
   * @default 'name'
   */
  conflictProperty: undefined,
  
  /**
   * If specified, record with this diff ID will not have a conflict label assigned,
   * because this record will be considered as a default in its namespace.
   * @type {string|undefined}
   */
  defaultId: undefined,
    
  init() {
    this._super(...arguments);
    let {
      diffProperty,
      conflictProperty,
    } = this.getProperties('content', 'conflictProperty', 'diffProperty');
    
    if (!diffProperty) {
      this.set('diffProperty', 'id');
      diffProperty = 'id';
    }
    if (!conflictProperty) {
      this.set('conflictProperty', 'name');
      conflictProperty = 'name';
    }
    
    /**
     * Assigns a ``conflictLabel`` property for each record in array.
     * It distinguish a record within other records if there are multiple
     * records with the same name.
     */
    let computeConflictIds = observer('content.[]', `content.@each.{${diffProperty},${conflictProperty}}`, 'defaultId', function() {
      let {
        content: records,
        diffProperty,
        conflictProperty,
        defaultId,
      } = this.getProperties('content', 'conflictProperty', 'diffProperty', 'defaultId');
      
      if (isArray(records)) {
        addConflictLabels(records, conflictProperty, diffProperty, defaultId);
      }
    });
      
    this.reopen({ computeConflictIds });
    this.computeConflictIds();
  },
});
