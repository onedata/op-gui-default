/**
 * Various stats for transfer.
 *
 * @module models/space-transfer-stat
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

const {
  Model,
  belongsTo,
} = DS;

export default Model.extend({
  minuteStat: belongsTo('space-transfer-time-stat'),
  hourStat: belongsTo('space-transfer-time-stat'),
  dayStat: belongsTo('space-transfer-time-stat'),
  monthStat: belongsTo('space-transfer-time-stat'),
});
