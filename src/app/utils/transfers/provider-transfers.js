import Ember from 'ember';

import _ from 'lodash';

const {
  get,
} = Ember;

/**
 * @typedef {Object} ProviderTransfer
 * @property {string} src ID of transfer source provider
 * @property {string} dest ID of transfer desitnation provider
 * @property {number} bytesPerSec recent tranfer speed: B/s
 */

/**
 * Collection of:
 * ```
 * {
 *   src: 'providerId1',
 *   dest: 'providerId2',
 *   bytesPerSec: 1000, // B/s
 * }
 * ```
 * Each vector (source, dest) can occur only once! (a,b or b,a) can occur at the same time
 * @returns {Array<ProviderTransfer>}
 */
export default function providerTransfers(transfers) {  
  const result = [];
  _.forEach(
    _.groupBy(transfers, t => get(t, 'destination')), 
    (dtrans, dest) => {
      const bySource = {};
      // dtrans - array of transfers for destination === dest
      dtrans.forEach(dt => {
        // obj: src provider -> last minute bytes
        const hstats = dt.stats.hour;
        for (let src in hstats) {
          if (!bySource[src]) {
            bySource[src] = 0;
          }
          bySource[src] += _.last(hstats[src]);
        }
      });
      _.forEach(bySource, (totalBytes, src) => {
        result.push({
          dest,
          src,
          bytesPerSec: totalBytes/60,
        });
      });
  });
  return result;
}