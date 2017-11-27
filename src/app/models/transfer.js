import DS from 'ember-data';
import Ember from 'ember';
import _ from 'lodash';

import TransferRuntimeMixin from 'op-worker-gui/mixins/models/transfer-runtime';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  Model,
  attr,
  belongsTo,
} = DS;

const {
  computed,
} = Ember;

export default Model.extend(TransferRuntimeMixin, {
  /**
   * One of:
   * - scheduled
   * - active
   * - skipped
   * - completed
   * - cancelled
   * - failed
   */
  status: attr('string'),
  
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
   * Absolute file or directory path that is transferred
   */
  path: attr('string'),
  
  /**
   * One of: dir, file, deleted, unknown
   */
  fileType: attr('string'),
  
  startTime: attr('number'),
  
  /**
   * Non-empty only if transfer is not ongoing (`isOngoing`)
   */
  finishTime: attr('number'),
  
  systemUserId: attr('string'),
  
  currentStat: belongsTo('transfer-current-stat'),

  minuteStat: belongsTo('transfer-time-stat'),
  hourStat: belongsTo('transfer-time-stat'),
  dayStat: belongsTo('transfer-time-stat'),
  monthStat: belongsTo('transfer-time-stat'),
  
  /**
   * Id of space that this transfer belongs to
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
    function () {
      return this.get('isLoaded') &&
        this.get('currentStat.isLoaded') &&
        this.get('systemUser.isLoaded');
    }
  ),
  
  dest: computed.reads('destination'),
  bytesPerSec: computed.reads('currentStat.bytesPerSec'),
  userName: computed.reads('systemUser.name'),
  transferredBytes: computed.reads('currentStat.transferredBytes'),
  transferredFiles: computed.reads('currentStat.transferredFiles'),
  
  isOngoing: computed('status', function () {
    return _.includes(['active', 'scheduled'], this.get('status'));
  }),
});
