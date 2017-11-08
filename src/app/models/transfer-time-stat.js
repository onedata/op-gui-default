import DS from 'ember-data';

const {
  Model,
  attr,
} = DS;

/**
 * FIXME:
 * @module models/transfer-time-stat
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Model.extend({
  inProgress: attr('boolean'),
  bytesTransferred: attr('number'),
  bytesPerSec: attr('number'),
});

// --- FIXME: mock ---

import _ from 'lodash';
 
const ONE_GB = Math.pow(1024, 3);

export function mockRecord(
  period
) {
  let count;
  switch (period) {
    case 'minute':
      count = 12;
      break;
    case 'hour':
      count = 60;
      break;
    case 'day':
      count = 24;
      break;
    default:
      throw new Error('bad period type: ' + period);
  }
  return {
    date: new Date().toISOString(),
    [period]: {
      p1: _.range(count).map(i => Math.floor(i * 0.1 * ONE_GB)),
      p2: _.range(count).map(i => Math.floor(i * 0.2 * ONE_GB)),
    },
  };
}
