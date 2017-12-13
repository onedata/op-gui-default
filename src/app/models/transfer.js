import DS from 'ember-data';
import Ember from 'ember';

import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  Model,
  attr,
  belongsTo,
} = DS;

const {
  computed,
} = Ember;

export default Model.extend({
  /**
   * Id of Provider that is destination of this transfer
   */
  destination: attr('string'),
  
  file: belongsTo('file', { async: true, inverse: null }),
  
  /**
   * If true, the transfer is a migration, so the file will be invalidated
   * on `migrationSource` provider after migration completion
   */
  migration: attr('boolean'),
  
  /**
   * Id of provider that will invalidate the file after transfer
   */
  migrationSource: attr('string'),
  
  /**
   * If true, the transfer is in progress (should be in current transfers collection)
   */
  isOngoing: attr('boolean'),
  
  /**
   * Absolute file or directory path that is transferred
   */
  path: attr('string'),
  
  /**
   * One of: dir, file, deleted, unknown
   */
  fileType: attr('string'),
  
  /**
   * UNIX timestamp seconds format
   */
  startTime: attr('number'),
  
  /**
   * Non-empty only if transfer is not current (`isCurrent`)
   * UNIX timestamp seconds format
   */
  finishTime: attr('number'),
  
  systemUserId: attr('string'),
  
  currentStat: belongsTo('transfer-current-stat'),

  minuteStat: belongsTo('transfer-time-stat'),
  hourStat: belongsTo('transfer-time-stat'),
  dayStat: belongsTo('transfer-time-stat'),
  monthStat: belongsTo('transfer-time-stat'),
  
  status: computed.reads('currentStat.status'),
  
  /**
   * Space in which this transfer is done
   */
  space: belongsTo('space', { async: true, inverse: null }),
  
  /**
   * @type {Ember.ComputedProperty<PromiseObject<SystemProvider>>}
   */
  systemUser: computed('space', 'systemUserId', function () {
    const systemUserId = this.get('systemUserId');
    // the relation id should exist always if record exists
    const spaceId = this.belongsTo('space').id();
    if (spaceId != null) {
      return PromiseObject.create({
        promise: this.store.queryRecord('system-user', {
          id: systemUserId,
          context: {
            od_space: spaceId,
          },
        })
      }); 
    }
  }),
  
  // Flattening some properties for view
  tableDataIsLoaded: computed(
    'isLoaded',
    'currentStat.isLoaded',
    'systemUser.isLoaded',
    '_completedReloading',
    function () {
      return this.get('isLoaded') &&
        this.get('currentStat.isLoaded') &&
        this.get('systemUser.isLoaded') &&
        !this.get('_completedReloading');
    }
  ),
  
  dest: computed.reads('destination'),
  bytesPerSec: computed.reads('currentStat.bytesPerSec'),
  userName: computed.reads('systemUser.name'),
  transferredBytes: computed.reads('currentStat.transferredBytes'),
  transferredFiles: computed.reads('currentStat.transferredFiles'),
  
  currentStatError: computed('currentStat.{isSettled,content}', function () {
    return this.get('currentStat.isSettled') && this.get('currentStat.content') == null;
  }),
  
  isCurrent: computed.reads('isOngoing'),
});
