/**
 * Reduce transfers data to create just pairs of current connections between providers
 * caused by transfers
 *
 * @module utils/transfers/provider-transfer-connections
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
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
