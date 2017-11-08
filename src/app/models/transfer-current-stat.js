import DS from 'ember-data';

const {
  Model,
  attr,
} = DS;

/**
 * FIXME:
 * @module models/transfer-current-stat
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Model.extend({
  timestamp: attr('number'),
  transferredBytes: attr('number'),
  bytesPerSec: attr('object'),
});

// --- FIXME: mock ---

export function mockRecord(
  inProgress,
  bytesTransferred,
  bytesPerSec
) {
  return {
    date: new Date().toISOString(),
    inProgress,
    bytesTransferred,
    bytesPerSec: {
      p1: bytesPerSec,
      p2: 2*bytesPerSec,
    },
  };
}
