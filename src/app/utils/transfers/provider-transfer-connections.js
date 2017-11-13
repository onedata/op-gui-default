/**
 * FIXME: jsdoc
 */

import _ from 'lodash';

/**
 * Generates collection of connection between two providers
 * Order in connection is random; each pair can occur once.
 * @param {Array<ProviderTransfer>} providerTransfers 
 * @return {Array<Array>} each array element has 2 elements, eg.
 *    `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
 */
export default function transfersProviderConnections(providerTransfers) {
  return _(providerTransfers)
    .map(itran => [itran.src, itran.dest].sort())
    .uniqWith((a, b) => _.isEqual(a, b))
    .value();
}
